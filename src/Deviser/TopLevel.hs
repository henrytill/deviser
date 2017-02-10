{-# LANGUAGE OverloadedStrings #-}

module Deviser.TopLevel where

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

-- Top-level

evalInTopLevel :: LispVal -> Eval LispVal
evalInTopLevel (List (Atom "define" : [Atom var, expr])) = do
  primitiveEnv <- ask
  topLevelEnv  <- get
  evaledExpr   <- local (const (primitiveEnv <> topLevelEnv)) (eval expr)
  put (Map.insert var evaledExpr topLevelEnv)
  return (Atom var)
evalInTopLevel expr = do
  primitiveEnv <- ask
  topLevelEnv  <- get
  put topLevelEnv
  local (const (topLevelEnv <> primitiveEnv)) (eval expr)

runEval :: EnvCtx -> EnvCtx -> Eval b -> IO (Either LispError (b, EnvCtx))
runEval primitiveEnv topLevelEnv action =
  runExceptT (runReaderT (runStateT (unEval action) topLevelEnv) primitiveEnv)


-- Files

evalInTopLevelWrapper :: LispVal -> Eval [LispVal]
evalInTopLevelWrapper (List exprs) = mapM evalInTopLevel exprs
evalInTopLevelWrapper x            = throwError (BadSpecialForm "read" x)

basicReadFile :: LispVal -> Eval LispVal
basicReadFile (String filePath) = do
  file   <- liftIO $ readFile (T.unpack filePath)
  parsed <- pure (readExprFile (T.pack file))
  case parsed of
    Left err ->
      throwError (Syntax err)
    Right x -> do
      xs <- evalInTopLevelWrapper x
      liftIO $ mapM_ print xs
      return (last xs)
basicReadFile x = throwError (TypeMismatch "string" x)

readEvalFile :: EnvCtx -> T.Text -> IO (Either LispError ([LispVal], EnvCtx))
readEvalFile topLevelEnv contents =
  pure (readExprFile contents) >>= \parsed ->
  case parsed of
    Left err ->
      return (Left (Syntax err))
    Right x ->
      liftIO $ runEval neuPrim topLevelEnv (evalInTopLevelWrapper x)

readEvalPrintFile :: EnvCtx -> FilePath -> IO ()
readEvalPrintFile topLevelEnv filePath = do
  file   <- readFile filePath
  evaled <- readEvalFile topLevelEnv (T.pack file)
  case evaled of
    Left err ->
      print err
    Right (results, _) ->
      mapM_ print results

runFile :: FilePath -> IO ()
runFile = readEvalPrintFile (Map.fromList [])


-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

neuPrim :: EnvCtx
neuPrim = primEnv <> Map.fromList [("read", mkF (unaryOp basicReadFile))]

parseLoop :: MonadIO m => [T.Text] -> m LispVal
parseLoop previousInput = do
  currentInput <- fmap T.pack (liftIO getLine)
  let inputToParse = if Prelude.null previousInput
                     then [currentInput]
                     else previousInput ++ [currentInput]
  parsed       <- pure (readExpr (T.unlines inputToParse))
  case parsed of
    Left _ ->
      liftIO (putStrLn ("Parse buffer: " ++ show inputToParse)) >>
      parseLoop inputToParse
    Right expr ->
      liftIO (putStrLn ("Parse buffer: " ++ show inputToParse)) >>
      liftIO (putStrLn ("Parsed: " ++ show expr)) >>
      return expr

readEvalPrintInput :: EnvCtx -> IO ()
readEvalPrintInput topLevelEnv = do
  liftIO (flushStr "><> ")
  result <- parseLoop []
  evaled <- liftIO $ runEval neuPrim topLevelEnv (evalInTopLevel result)
  case evaled of
    Left err ->
      liftIO (print err) >>
      readEvalPrintInput topLevelEnv
    Right (res, newTopLevelEnv) ->
      liftIO (print res) >>
      readEvalPrintInput newTopLevelEnv

runREPL :: IO ()
runREPL =
  putStrLn "Welcome to Deviser" >>
  readEvalPrintInput (Map.fromList [])
