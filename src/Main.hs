module Main where

import System.Environment (getArgs)
import Deviser.TopLevel

main :: IO ()
main =
  getArgs >>= \args ->
  case args of
    []     -> runREPL
    [file] -> runFile file
    _      -> error "Too many arguments"
