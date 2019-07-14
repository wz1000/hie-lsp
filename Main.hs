{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                     as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Aeson                            as J
import           Data.Default
import qualified Data.HashMap.Strict                   as H
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                             as T
import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Types.MessageFuncs
import           Language.Haskell.LSP.Types (ClientMethod (..))
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
import           Language.Haskell.LSP.VFS
import           System.Exit
import qualified System.Log.Logger                     as L
import qualified Data.Rope.UTF16                       as Rope
import qualified Data.Dependent.Map                    as DM
import           Data.Dependent.Map (DMap, DSum(..))
import           Data.Functor.Product
import qualified Data.Map                             as M
import Data.Either
import Data.Foldable
import Data.Maybe

import HieDb
import GHC.Paths
import GHC
import HieTypes
import HieBin
import SrcLoc
import HieUtils
import GhcMake (downsweep)
import HscTypes
import StringBuffer (stringToStringBuffer)
import Outputable (showPpr)
import Name

import Data.Time.Clock
import System.Directory
import Data.IORef

import Reflex
import Reflex.Host.Basic

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  run (return ()) >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

run :: IO () -> IO Int
run dispatcherProc = flip E.catches handlers $ do
  rin  <- atomically newTChan :: IO (TChan FromClientMessage)
  let
    dp lf = do
      liftIO $ U.logs "main.run:dp entered"
      _rpid  <- forkIO $ do
        withHieDb "hie-lsp.hiedb" $ \db -> runLSPServer rin lf db net
      liftIO $ U.logs "main.run:dp tchan"
      dispatcherProc
      liftIO $ U.logs "main.run:dp after dispatcherProc"
      return Nothing
    initial = Core.InitializeCallbacks (const $ Right ()) (const $ Right ()) dp

  flip E.finally finalProc $ do
      Core.setupLogger (Just "/tmp/lsp-hello.log") [] L.DEBUG
      CTRL.run initial (lspHandlers rin) lspOptions (Just "/tmp/lsp-hello-session.log")

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

type LSPServer t m = BasicGuestConstraints t m
                  => Core.LspFuncs ()
                  -> HieDb
                  -> Event t FromClientMessage
                  -> BasicGuest t m ()

data ClientMessageWithResponse m = ClientMessageWithResponse (J.ClientMessage m) (Core.ClientResponseHandler m)
type FromClientMessage = DMap J.SClientMethod ClientMessageWithResponse

runLSPServer :: TChan FromClientMessage -> Core.LspFuncs () -> HieDb -> (forall t m. LSPServer t m) -> IO ()
runLSPServer rin lf db network = do
  let isQuit = const False -- NotExit{} = True
  basicHostWithQuit $ do
    (reqev, sendReq) <- newTriggerEvent
    void $ liftIO $ forkIO $ forever $
      (atomically $ readTChan rin) >>= sendReq
    network lf db reqev
    let quitEv = void $ ffilter isQuit reqev
    pure quitEv


type family Res cm where
  Res (J.RequestMessage m req rsp) = Either J.ResponseError rsp
  Res (J.SomeMessage m) = Maybe (Either J.ResponseError J.Value)
  Res _ = ()

type ServerRes m = Res (J.ClientMessage m)
type ClientRes m = Res (J.ServerMessage m)

handleMessage_
  :: (PerformEvent t m, MonadIO (Performable m))
  => EventSelectorG t J.SClientMethod ClientMessageWithResponse
  -> J.SClientMethod cm
  -> (J.ClientMessage cm -> Performable m (ServerRes cm))
  -> m ()
handleMessage_ sel m act =
  performEvent_ $ selectG sel m <&> (void . respondMessage m (fmap ((),) . act))

handleMessage
  :: (PerformEvent t m, MonadIO (Performable m))
  => EventSelectorG t J.SClientMethod ClientMessageWithResponse
  -> J.SClientMethod cm
  -> (J.ClientMessage cm -> Performable m (a,ServerRes cm))
  -> m (Event t a)
handleMessage sel m act =
  performEvent $ selectG sel m <&> respondMessage m act

respondMessage
  :: MonadIO m
  => J.SClientMethod cm
  -> (J.ClientMessage cm -> m (a,ServerRes cm))
  -> ClientMessageWithResponse cm
  -> m a
respondMessage m act (ClientMessageWithResponse mess sendResp) =
  case J.splitClientMethod m of
    J.IsClientNot -> fst <$> act mess
    J.IsClientReq -> do
      (a,res) <- act mess
      liftIO $ sendResp res
      pure a
    J.IsClientEither -> case mess of
      J.NotMess _ -> do
        (a,res) <- act mess
        case res of
          Nothing -> pure a
          Just _ -> do
            liftIO $ U.logs $ "Cannot respond to notification"
            pure a
      J.ReqMess _ -> do
        (a,mres) <- act mess
        case (mres, sendResp) of
          (Just res,Just sendResp') -> do
            liftIO $ sendResp' res
            pure a
          (Nothing,Just sendResp') -> liftIO $ do
            U.logs $ "need to respond to respond to request"
            sendResp' (Left $ J.ResponseError J.InternalError "Didn't get response from server" Nothing)
            pure a
          (_ , Nothing) -> error "Didn't get response handler from haskell-lsp"

type family EvRes t cm where
  EvRes t (J.RequestMessage m req rsp) = Event t (Either J.ResponseError rsp)
  EvRes t (J.SomeMessage m) = Maybe (Event t (Either J.ResponseError J.Value))
  EvRes t _ = ()

type EvClientRes t m = EvRes t (J.ServerMessage m)
makeClientRequest
  :: (MonadIO m, TriggerEvent t m, Reflex t)
  => Core.LspFuncs config
  -> J.SServerMethod sm -> J.ServerMessage sm -> m (EvClientRes t sm)
makeClientRequest lf m mess =
  case J.splitServerMethod m of
    J.IsServerNot ->
      liftIO $ Core.sendFunc lf $ Core.SomeServerMessageWithResponse m mess ()
    J.IsServerReq -> do
      (outEv, k) <- newTriggerEvent
      liftIO $ Core.sendFunc lf $ Core.SomeServerMessageWithResponse m mess k
      return outEv
    J.IsServerEither -> case mess of
      J.NotMess _ -> do
        liftIO $ Core.sendFunc lf $ Core.SomeServerMessageWithResponse m mess Nothing
        pure Nothing
      J.ReqMess _ -> do
        (outEv, k) <- newTriggerEvent
        liftIO $ Core.sendFunc lf $ Core.SomeServerMessageWithResponse m mess (Just k)
        return $ Just outEv


type HieFiles = M.Map J.NormalizedUri (HieFile, UTCTime, FilePath)

data IdeState
  = IdeState
  { getVfs :: VFS
  , getEnv :: HscEnv
  , getHieFiles :: HieFiles
  }

initState :: IO IdeState
initState = do
  hsc <- runGhc (Just libdir) getSession
  return $ IdeState M.empty hsc M.empty

updateState :: Core.LspFuncs config -> HieDb -> IdeState -> IO IdeState
updateState lf db old = do
  vfs <- Core.getVirtualFilesFunc lf
  env <- updateModuleGraph vfs (getEnv old)
  hieFiles <- updateHieFiles db vfs env (getHieFiles old)
  return $ IdeState vfs env hieFiles

getVFSTargets :: VFS -> [Target]
getVFSTargets = concatMap go . M.toList
  where
    go (uri,vf)
      | Nothing <- J.uriToFilePath $ J.fromNormalizedUri uri = []
      | Just fp <- J.uriToFilePath $ J.fromNormalizedUri uri =
          [Target (TargetFile fp Nothing) False (Just (buf, _modtime vf))]
        where buf = stringToStringBuffer $ Rope.toString $ _text vf

updateModuleGraph :: VFS -> HscEnv -> IO HscEnv
updateModuleGraph vfs env = do
  let targets = getVFSTargets vfs
      old_graph = hsc_mod_graph env
      env' = env { hsc_targets = targets }
  mss <- downsweep env' (mgModSummaries old_graph) [] False
  let new_graph = mkModuleGraph $ rights mss
  U.logs $  "got module graph" ++ showPpr (hsc_dflags env') (rights mss)
  return $ env' { hsc_mod_graph = new_graph }

getTimeIfExists :: FilePath -> IO (Maybe UTCTime)
getTimeIfExists fp = do
  exists <- doesFileExist fp
  if exists
  then Just <$> getModificationTime fp
  else pure Nothing

updateHieFiles :: HieDb -> VFS -> HscEnv -> HieFiles -> IO HieFiles
updateHieFiles db vfs env oldfiles = do
  updated <- flip M.traverseMaybeWithKey oldfiles $ \uri (file,time,path) -> do
    if M.notMember uri vfs
    then pure Nothing
    else do
      mdiskTime <- getTimeIfExists path
      case mdiskTime of
        Nothing -> pure Nothing
        Just diskTime
          | diskTime <= time -> pure $ Just (file,time, path)
          | otherwise -> do
              nc <- readIORef $ hsc_NC env
              (result,nc') <- readHieFile nc path
              writeIORef (hsc_NC env) nc'
              pure $ Just (hie_file_result result, diskTime, path)
  new <- flip M.traverseMaybeWithKey (vfs M.\\ oldfiles) $ \uri _ -> do
    case getModule env (J.fromNormalizedUri uri) of
      Nothing -> pure Nothing
      Just mdl -> do
        mbrow <- lookupHieFile db (moduleName mdl) (moduleUnitId mdl)
        case mbrow of
          Nothing -> pure Nothing
          Just row -> do
            let path = hieModuleHieFile row
            mtime <- getTimeIfExists path
            case mtime of
              Nothing -> pure Nothing
              Just time -> do
                nc <- readIORef $ hsc_NC env
                (result,nc') <- readHieFile nc path
                writeIORef (hsc_NC env) nc'
                pure $ Just (hie_file_result result, time, path)
  pure (M.union new updated)

getModule :: HscEnv -> J.Uri -> Maybe Module
getModule env (J.uriToFilePath -> Just fp) = ms_mod <$> find go mss
  where mg = hsc_mod_graph env
        mss = mgModSummaries mg
        go ms = ml_hs_file (ms_location ms) == Just fp
getModule _ _ = Nothing

getModFile :: IdeState -> Module -> Maybe J.Uri
getModFile s mdl = J.filePathToUri <$> (ml_hs_file . ms_location =<< mgLookupModule mg mdl)
  where mg = hsc_mod_graph $ getEnv s

getStateDyn
  :: (PerformEvent t m, MonadIO (Performable m)
  , MonadHold t m, MonadIO m, MonadSample t (Performable m), MonadFix m)
  => Core.LspFuncs config
  -> HieDb
  -> EventSelectorG t J.SClientMethod ClientMessageWithResponse
  -> m (Dynamic t IdeState)
getStateDyn lf db sel = do
  state <- liftIO initState
  mdo
    e <- performEvent $ leftmost [ () <$ selectG sel J.STextDocumentDidOpen
                                 , () <$ selectG sel J.STextDocumentDidChange
                                 , () <$ selectG sel J.STextDocumentDidSave
                                 , () <$ selectG sel J.STextDocumentDidClose
                                 ] <&> \() -> sample (current dyn) >>= liftIO . updateState lf db
    dyn <- holdDyn state e
    return dyn

net :: LSPServer t m
net lf db inEv = mdo
  let inSel = fanG inEv
      nextLspReqId = liftIO $ Core.getNextReqId lf

  state <- getStateDyn lf db inSel

  clickEv' <- handleMessage inSel J.SInitialized $ \_mess -> do
    liftIO $ U.logm "****** reactor: processing Initialized Notification"
    rid <- nextLspReqId
    let
      params = J.ShowMessageRequestParams J.MtWarning "choose an option"
                 (Just [J.MessageActionItem "vanilla", J.MessageActionItem "chocolate"])
    click <- makeClientRequest lf J.SWindowShowMessageRequest (fmServerShowMessageRequest rid params)
    pure (click,())
  clickEv <- switchHold never clickEv'

  otherClick' <- performEvent $ leftmost [clickEv, otherClick] <&> \(Right mchoice) -> do
    let text = case mchoice of
          Nothing -> "You didn't choose anything :("
          Just c -> "You chose " <> c ^. J.title <> "!"
    makeClientRequest lf J.SWindowShowMessage $ fmServerShowMessageNotification J.MtWarning text
    let rparams = J.ShowMessageRequestParams J.MtWarning "choose an option"
                 (Just [J.MessageActionItem "chocolate", J.MessageActionItem "vanilla"])
    rid <- nextLspReqId
    makeClientRequest lf J.SWindowShowMessageRequest (fmServerShowMessageRequest rid rparams)
  otherClick <- switchHold never otherClick'

  handleMessage_ inSel J.STextDocumentHover $ \params -> do
    let uri = params ^. J.params . J.textDocument . J.uri
        pos = params ^. J.params . J.position
        line = pos ^. J.line
        char = pos ^. J.character
        dmess = "Hover at " <> T.pack (show (line,char))
        hover mess = J.Hover (J.HoverContents (J.MarkupContent J.MkPlainText mess)) Nothing
    curState <- sample (current state)
    let mhf = M.lookup (J.toNormalizedUri uri) (getHieFiles curState)
        dflags = hsc_dflags $ getEnv curState
    case mhf of
      Nothing -> pure (Right $ Just $ hover dmess)
      Just (hf,_,_) -> do
        let types = concat $ pointCommand hf (line+1, char+1) Nothing $ nodeType . nodeInfo
            msgs = map (T.pack . renderHieType dflags . flip recoverFullType (hie_types hf)) types
            msg = T.intercalate "\n\n" msgs
        pure (Right $ Just $ hover msg)

  handleMessage_ inSel J.STextDocumentDefinition $ \params -> do
    let uri = params ^. J.params . J.textDocument . J.uri
        pos = params ^. J.params . J.position
        line = pos ^. J.line
        char = pos ^. J.character
        noLoc = Right $ J.MultiLoc []
    curState <- sample (current state)
    let location mdl sp
          | Nothing <- getModFile curState mdl = []
          | Just uri <- getModFile curState mdl = [J.Location uri (J.Range start end)]
          where
            start = J.Position (srcSpanStartLine sp - 1) (srcSpanStartCol sp -1)
            end = J.Position (srcSpanEndLine sp - 1) (srcSpanEndCol sp -1)
    let mhf = M.lookup (J.toNormalizedUri uri) (getHieFiles curState)
    case mhf of
      Nothing -> pure noLoc
      Just (hf,_,_) -> do
        let names = concat $ pointCommand hf (line+1, char+1) Nothing $ rights . M.keys . nodeIdentifiers . nodeInfo
        locs <- forM names $ \name ->
                  case nameSrcSpan name of
                    RealSrcSpan dsp ->
                      pure $ location (fromMaybe (hie_module hf) (nameModule_maybe name)) dsp
                    _ -> case nameModule_maybe name of
                          Nothing -> pure []
                          Just mod -> do
                            def <- liftIO $ findDef db (nameOccName name) (moduleName mod) (Just $ moduleUnitId mod)
                            case def of
                              Left _ -> pure []
                              Right (dsp, mdl) -> pure $ location mdl dsp
        pure $ Right $ J.MultiLoc $ concat locs

  handleMessage_ inSel J.STextDocumentReferences $ \params -> do
    let uri = params ^. J.params . J.textDocument . J.uri
        pos = params ^. J.params . J.position
        line = pos ^. J.line
        char = pos ^. J.character
        noLoc = Right $ J.List []
    curState <- sample (current state)
    let location mdl sp
          | Nothing <- getModFile curState mdl = []
          | Just uri <- getModFile curState mdl = [J.Location uri (J.Range start end)]
          where
            start = J.Position (srcSpanStartLine sp - 1) (srcSpanStartCol sp -1)
            end = J.Position (srcSpanEndLine sp - 1) (srcSpanEndCol sp -1)
        rowToLoc x = location mdl sp
          where
            mdl = mkModule (refSrcUnit x) (refSrcMod x)
            sp = mkRealSrcSpan st en
            st = mkRealSrcLoc "" (refSLine x) (refSCol x)
            en = mkRealSrcLoc "" (refELine x) (refECol x)
    let mhf = M.lookup (J.toNormalizedUri uri) (getHieFiles curState)
    case mhf of
      Nothing -> pure noLoc
      Just (hf,_,_) -> do
        let names = concat $ pointCommand hf (line+1, char+1) Nothing $ rights . M.keys . nodeIdentifiers . nodeInfo
        locs <- forM names $ \name ->
                  case nameModule_maybe name of
                    Just mod -> do
                      rows <- liftIO $ search db (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod)
                      pure $ concatMap rowToLoc rows
                    Nothing -> do
                      let refmap = generateReferencesMap (getAsts $ hie_asts hf)
                          refs = map (location (hie_module hf) . fst) $ fromMaybe [] $ M.lookup (Right name) refmap
                      pure $ concat refs
        pure $ Right $ J.List $ concat locs
  pure ()

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspOptions :: Core.Options
lspOptions = def { Core.textDocumentSync = Just syncOptions
                 , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
                 }

lspHandlers :: TChan FromClientMessage -> Core.Handlers
lspHandlers rin
  = DM.fromList
  [ mkHandler rin J.SInitialized
  , mkHandler rin J.STextDocumentDidOpen
  , mkHandler rin J.STextDocumentDidChange
  , mkHandler rin J.STextDocumentDidSave
  , mkHandler rin J.STextDocumentDidClose
  , mkHandler rin J.STextDocumentHover
  , mkHandler rin J.STextDocumentDefinition
  , mkHandler rin J.STextDocumentReferences
  ]

mkHandler :: TChan FromClientMessage -> J.SClientMethod m -> DSum J.SClientMethod Core.Handler
mkHandler rin m =
  m :=> Core.Handler (\mess res -> atomically $ writeTChan rin (DM.singleton m (ClientMessageWithResponse mess res)))

-- ---------------------------------------------------------------------
