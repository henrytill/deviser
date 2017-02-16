{-# LANGUAGE OverloadedStrings #-}

module Deviser.TopLevel where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.IO
import Deviser.Evaluator
import Deviser.Parser (readExpr, readExprFile)
import Deviser.Types

-- Top-level

evalInTopLevel :: LispVal -> Eval LispVal
evalInTopLevel (List (Atom "define" : [Atom var, expr])) = do
  topLevelEnv <- get
  evaledExpr  <- local (mappend topLevelEnv) (expand expr >>= eval)
  put (Map.insert var evaledExpr topLevelEnv)
  return (Atom var)
evalInTopLevel expr = do
  topLevelEnv <- get
  put topLevelEnv
  local (mappend topLevelEnv) (expand expr >>= eval)

runEval :: EnvCtx -> EnvCtx -> Eval b -> IO (Either LispError (b, EnvCtx))
runEval primitiveEnv topLevelEnv action =
  runExceptT (runReaderT (runStateT (unEval action) topLevelEnv) primitiveEnv)

basicEnv :: EnvCtx
basicEnv = primEnv <> Map.fromList [ ("load",         mkF (unaryOp loadFile))
                                   , ("file-exists?", mkF (unaryOp fileExists))
                                   ]


-- Files

evalInTopLevelWrapper :: LispVal -> Eval [LispVal]
evalInTopLevelWrapper (List exprs) = mapM evalInTopLevel exprs
evalInTopLevelWrapper x            = throwError (BadSpecialForm "read" x)

fileExists :: LispVal -> Eval LispVal
fileExists (String s) = Bool <$> liftIO (doesFileExist (T.unpack s))
fileExists x          = throwError (TypeMismatch "string" x)

loadFile :: LispVal -> Eval LispVal
loadFile f@(String filePath) = do
  (Bool exists) <- fileExists f
  if exists
    then liftIO (readFile (T.unpack filePath)) >>= \file ->
    case readExprFile (T.pack file) of
      Left err ->
        throwError (Syntax err)
      Right x -> do
        xs <- evalInTopLevelWrapper x
        liftIO $ mapM_ print xs
        return (last xs)
    else throwError (Default "file not found")
loadFile x = throwError (TypeMismatch "string" x)

readEvalFile :: EnvCtx -> T.Text -> IO (Either LispError ([LispVal], EnvCtx))
readEvalFile topLevelEnv contents =
  case readExprFile contents of
    Left err ->
      return (Left (Syntax err))
    Right x ->
      liftIO $ runEval basicEnv topLevelEnv (evalInTopLevelWrapper x)

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

parseLoop :: MonadIO m => [T.Text] -> m LispVal
parseLoop previousInput = do
  currentInput <- T.pack <$> liftIO getLine
  let inputToParse = if Prelude.null previousInput
                     then [currentInput]
                     else previousInput ++ [currentInput]
  case readExpr (T.unlines inputToParse) of
    Left _ ->
      -- liftIO (putStrLn ("Parse buffer: " ++ show inputToParse)) >>
      parseLoop inputToParse
    Right expr ->
      -- liftIO (putStrLn ("Parse buffer: " ++ show inputToParse)) >>
      -- liftIO (putStrLn ("Parsed: " ++ show expr)) >>
      return expr

readEvalPrintInput :: EnvCtx -> IO ()
readEvalPrintInput topLevelEnv = do
  liftIO (flushStr "><> ")
  result <- parseLoop []
  evaled <- liftIO $ runEval basicEnv topLevelEnv (evalInTopLevel result)
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
