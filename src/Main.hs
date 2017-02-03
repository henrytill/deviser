module Main where

import System.Environment (getArgs)
import Deviser.REPL

main :: IO ()
main =
    getArgs >>= \args ->
    if null args
    then runREPL
    else runOne args
