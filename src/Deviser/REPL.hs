module Deviser.REPL where

import Control.Monad.Except (unless)
import Deviser.Types
import Deviser.Parser
import System.IO
import Deviser.Evaluator

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrowsError (fmap show (liftThrows (readExpr expr) >>= eval env))

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
    result <- prompt
    unless (predicate result) (action result >> until_ predicate prompt action)

runOne :: [String] -> IO ()
runOne args =
    primitiveBindings >>=
    flip bindVars [("args", List (map String (tail args)))] >>= \env ->
    runIOThrowsError (fmap show (eval env (List [Atom "load", String (head args)]))) >>=
    putStrLn

runREPL :: IO ()
runREPL =
    primitiveBindings >>=
    until_ (== "quit") (readPrompt ">>> ") . evalAndPrint
