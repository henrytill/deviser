{-# LANGUAGE OverloadedStrings #-}

module Deviser.REPL where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import System.IO
import Deviser.Evaluator
import Deviser.Parser (readExpr, readExprFile)
import Deviser.Types

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalInTopLevel :: LispVal -> Eval LispVal
evalInTopLevel fullExpr @ (List (Atom "define" : [Atom var, expr])) = do
  primitiveEnv <- ask
  topLevelEnv  <- get
  evaledExpr   <- local (const (primitiveEnv <> topLevelEnv)) (eval expr)
  result       <- local (const (primitiveEnv <> topLevelEnv)) (eval fullExpr)
  put (Map.insert var evaledExpr topLevelEnv)
  return result
evalInTopLevel expr = do
  primitiveEnv <- ask
  topLevelEnv  <- get
  put topLevelEnv
  local (const (primitiveEnv <> topLevelEnv)) (eval expr)

runEval :: EnvCtx -> Eval b -> IO (Either LispError (b, EnvCtx))
runEval env action = runExceptT (runReaderT (runStateT (unEval action) (Map.fromList [])) env)

parseLoop :: MonadIO m => [T.Text] -> m LispVal
parseLoop previousInput = do
  currentInput <- fmap T.pack (liftIO getLine)
  let inputToParse = if Prelude.null previousInput
                     then [currentInput]
                     else previousInput ++ [currentInput]
  parsed       <- pure (readExpr (T.unlines inputToParse))
  case parsed of
    Left _ ->
      liftIO (print inputToParse) >>
      parseLoop inputToParse
    Right expr ->
      liftIO (print inputToParse) >>
      return expr

readEvalPrint :: EnvCtx -> IO ()
readEvalPrint env = do
  liftIO (flushStr "><> ")
  result <- parseLoop []
  evaled <- liftIO $ runEval env (evalInTopLevel result)
  case evaled of
    Left err ->
      liftIO (print err) >>
      readEvalPrint env
    Right (res, newEnv) ->
      liftIO (print res) >>
      readEvalPrint (newEnv <> env)

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList primEnv

runREPL :: IO ()
runREPL = readEvalPrint basicEnv
