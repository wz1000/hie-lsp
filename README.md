# hie-lsp

## Building

With GHC 8.8

```bash
$ cabal new-configure -w <ghc-8.8> --allow-newer
$ cabal new-install
```

## Features

- Type at point on hover
- Go to definition
- References
- World's most annoying popup message

## Usage

Go the directory of a some cabal project and run:

```bash
$ cd some-project-dir
$ cabal new-configure -w <ghc-8.8> --write-ghc-environment-file=always --ghc-options="-fwrite-ide-info"
```

`hie-lsp` needs an environment file to pick up your local packages and
construct a module graph.

Now, you need to generate a database with `hiedb` for `hie-lsp` to work.

The database needs to be called `hie-lsp.hiedb` and needs to exist in the
root directory of your project

```bash
$ cd hie-lsp/hiedb
$ cabal new-configure -w <ghc-8.8> --allow-newer
$ cabal new-build
$ cabal new-run hiedb -- -D some-project-dir/hie-lsp.hiedb index some-project-dir
```

With VSCode, install https://github.com/wz1000/vscode-hie-server

In the `vscode-hie-server` settings, choose the option to use a "Use Custom
Hie Wrapper" and set the "Use Custom Hie Wrapper Path" to a script containing:

```bash
#!/bin/sh
hie-lsp
```

## Refreshing Info

To refresh the info from which queries are answered, you need to index your project again with
`hiedb`:

```bash
$ cabal new-run hiedb -- -D some-project-dir/hie-lsp.hiedb index some-project-dir
```

For `hie-lsp` to pick up the changes, make a trivial change to some file,
like adding a space, or opening a new file.
