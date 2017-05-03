# deviser

[![Build Status](https://travis-ci.org/henrytill/deviser.svg?branch=master)](https://travis-ci.org/henrytill/deviser)

`deviser` is a small Lisp-1 interpreter.

## Requirements

To build and use `deviser`, you will need either:

* [Nix](http://nixos.org/nix/) and the [cabal2nix](https://github.com/NixOS/cabal2nix) tool (**recommended**)

  or

* [Cabal](https://www.haskell.org/cabal/)

## Usage

### with Nix

```sh
$ cd deviser
$ cabal2nix --shell . >shell.nix
$ nix-shell
  ...
[nix-shell:~/path/to/deviser]$ cabal configure --enable-tests
  ...
[nix-shell:~/path/to/deviser]$ cabal build
  ...
[nix-shell:~/path/to/deviser]$ cabal test
  ...
[nix-shell:~/path/to/deviser]$ cabal run
Preprocessing library deviser-0.1.0.0...
Preprocessing executable 'deviser' for deviser-0.1.0.0...
Running deviser...
Welcome to Deviser
><> (define add1 (lambda (x) (+ x 1)))
add1
><> (add1 41)
42
><>

```

### with Cabal

```sh
$ cd deviser
$ cabal sandbox init
  ...
$ cabal configure --enable-tests
  ...
$ cabal build
  ...
$ cabal test
  ...
$ cabal run
Preprocessing library deviser-0.1.0.0...
Preprocessing executable 'deviser' for deviser-0.1.0.0...
Running deviser...
Welcome to Deviser
><> (define add1 (lambda (x) (+ x 1)))
add1
><> (add1 41)
42
><>
```

## Features

See [examples](examples).

## Running deviser inside Emacs

```elisp
(run-scheme "<path/to/deviser/executable>/deviser")
```

or

```elisp
(setq scheme-program-name "<path/to/deviser/executable>/deviser")
(run-scheme)
```
