{-# LANGUAGE OverloadedStrings #-}
module Deviser.REPL where

import Control.Monad.Except (unless)
import qualified Data.Text as T
import Deviser.Types
import Deviser.Parser
import System.IO
import Deviser.Evaluator

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO T.Text
readPrompt prompt = fmap T.pack (flushStr prompt >> getLine)

evalString :: Env -> T.Text -> IO T.Text
evalString env expr = runIOThrowsError (fmap (T.pack . show) (liftThrows (readExpr expr) >>= eval env))

evalAndPrint :: Env -> T.Text -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn . T.unpack

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  unless (predicate result) (action result >> until_ predicate prompt action)

runOne :: [String] -> IO ()
runOne args =
  primitiveBindings >>=
  flip bindVars [("args", List (map (String . T.pack) (tail args)))] >>= \env ->
  runIOThrowsError (fmap (T.pack . show) (eval env (List [Atom "load", String (head (map T.pack args))]))) >>=
  putStrLn . T.unpack

runREPL :: IO ()
runREPL =
  primitiveBindings >>=
  until_ (== "quit") (readPrompt ">>> ") . evalAndPrint
