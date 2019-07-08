{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GADTs               #-}

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
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
import           Language.Haskell.LSP.VFS
import           System.Exit
import qualified System.Log.Logger                     as L
import qualified Data.Rope.UTF16                       as Rope

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
      _rpid  <- forkIO $ runLSPServer rin lf net
      liftIO $ U.logs "main.run:dp tchan"
      dispatcherProc
      liftIO $ U.logs "main.run:dp after dispatcherProc"
      return Nothing
    init = Core.InitializeCallbacks (const $ Right ()) (const $ Right ()) dp

  flip E.finally finalProc $ do
    Core.setupLogger (Just "/tmp/lsp-hello.log") [] L.DEBUG
    CTRL.run init (lspHandlers rin) lspOptions (Just "/tmp/lsp-hello-session.log")

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

type LSPServer t m = BasicGuestConstraints t m
                  => IO J.LspId
                  -> Event t FromClientMessage
                  -> BasicGuest t m (Event t [FromServerMessage])

runLSPServer :: TChan FromClientMessage -> Core.LspFuncs () -> (forall t m. LSPServer t m) -> IO ()
runLSPServer rin lf network = do
  let isQuit NotExit{} = True
      isQuit _ = False
  basicHostWithQuit $ do
    (inev, k) <- newTriggerEvent
    void $ liftIO $ forkIO $ forever $ do
      inval <- atomically $ readTChan rin
      k inval
    oev <- network (Core.getNextReqId lf) inev
    performEvent_ $ oev <&> (mapM_ $ \msg ->
        liftIO $ Core.sendFunc lf msg)
    let quitEv = void $ ffilter isQuit inev
    pure ((),quitEv)

-- ---------------------------------------------------------------------

net :: LSPServer t m
net getNext iev =
  let nextLspReqId = liftIO getNext in
    performEvent $ iev <&> \ev -> case ev of
      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
      (RspFromClient rm) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm
        pure []

      -- -------------------------------

      (NotInitialized _notification) -> do
        liftIO $ U.logm "****** reactor: processing Initialized Notification"
        -- Server is ready, register any specific capabilities we need
        let
          registration = J.Registration "lsp-hello-registered" J.WorkspaceExecuteCommand Nothing
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId
        -- example of showMessageRequest
        let
          params = J.ShowMessageRequestParams J.MtWarning "choose an option for XXX"
                           (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
        rid1 <- nextLspReqId
        pure [ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations
             , ReqShowMessage $ fmServerShowMessageRequest rid1 params
             ]

      -- -------------------------------

      (NotDidOpenTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName =  J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        -- sendDiagnostics (J.toNormalizedUri doc) (Just 0)
        pure []

      -- -------------------------------

      (NotDidChangeTextDocument notification) -> do
        let doc :: J.NormalizedUri
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
                                 . to J.toNormalizedUri
        -- mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
        -- case mdoc of
        --   Just (VirtualFile _version str _) -> do
        --     liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: vf got:" ++ (show $ Rope.toString str)
        --   Nothing -> do
        --     liftIO $ U.logs "reactor:processing NotDidChangeTextDocument: vf returned Nothing"

        liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++ (show doc)
        pure []

      -- -------------------------------

      (NotDidSaveTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        -- sendDiagnostics (J.toNormalizedUri doc) Nothing
        pure []

      -- -------------------------------

      (ReqRename req) -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let
            _params = req ^. J.params
            _doc  = _params ^. J.textDocument . J.uri
            J.Position _l _c' = _params ^. J.position
            _newName  = _params ^. J.newName

        let we = J.WorkspaceEdit
                    Nothing -- "changes" field is deprecated
                    (Just (J.List [])) -- populate with actual changes from the rename
        let rspMsg = Core.makeResponseMessage req we
        pure [RspRename rspMsg]

      -- -------------------------------

      (ReqHover req) -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let J.TextDocumentPositionParams _doc pos = req ^. J.params
            J.Position _l _c' = pos

        let
          ht = Just $ J.Hover ms (Just range)
          ms = J.HoverContents $ J.markedUpContent "lsp-hello" "TYPE INFO"
          range = J.Range pos pos
        pure [RspHover $ Core.makeResponseMessage req ht]

      -- -------------------------------

      (ReqCodeAction req) -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            (J.List diags) = params ^. J.context . J.diagnostics

        let
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _l) = [J.Command title cmd cmdparams]
            where
              title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
              args = J.List
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
        let body = J.List $ map J.CACommand $ concatMap makeCommand diags
            rsp = Core.makeResponseMessage req body
        pure [RspCodeAction rsp]

      -- -------------------------------

      (ReqExecuteCommand req) -> do
        liftIO $ U.logs "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = req ^. J.params
            margs = params ^. J.arguments

        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs

        let
          reply v = RspExecuteCommand $ Core.makeResponseMessage req v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
          r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            lid <- nextLspReqId
            -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            pure [ reply (J.Object mempty)
                 , ReqApplyWorkspaceEdit $ fmServerApplyWorkspaceEditRequest lid we
                 ]
          Nothing ->
            pure [reply (J.Object mempty)]

      om -> do
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om
        pure []

-- ---------------------------------------------------------------------

toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
-- sendDiagnostics :: J.NormalizedUri -> Maybe Int -> R () ()
-- sendDiagnostics fileUri version = do
--   let
--     diags = [J.Diagnostic
--               (J.Range (J.Position 0 1) (J.Position 0 5))
--               (Just J.DsWarning)  -- severity
--               Nothing  -- code
--               (Just "lsp-hello") -- source
--               "Example diagnostic message"
--               (Just (J.List []))
--             ]
--   -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
--   publishDiagnostics 100 fileUri version (partitionBySource diags)

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
  = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin ReqRename
        , Core.hoverHandler                             = Just $ passHandler rin ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
        , Core.responseHandler                          = Just $ responseHandlerCb rin
        , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
        }

-- ---------------------------------------------------------------------

passHandler :: TChan FromClientMessage -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan FromClientMessage -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------
