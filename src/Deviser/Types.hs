{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Deviser.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Complex
import qualified Data.Map as Map
import Data.Ratio (numerator, denominator)
import qualified Data.Text as T
import Data.Typeable
import System.IO (Handle)
import Text.Parsec (ParseError)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String T.Text
  | Character Char
  | Bool Bool
  | PrimOp IFunc
  | Lambda IFunc EnvCtx
  | Port Handle
  | Nil
  deriving Typeable

type EnvCtx = Map.Map T.Text LispVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

newtype Eval a = Eval { unEval :: StateT EnvCtx (ReaderT EnvCtx (ExceptT LispError IO)) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadError LispError,
            MonadIO,
            MonadReader EnvCtx,
            MonadState EnvCtx)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Syntax ParseError
  | BadSpecialForm T.Text LispVal
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default T.Text


-- Show

unwordsList :: [LispVal] -> T.Text
unwordsList xs = T.unwords (showVal <$> xs)

showVal :: LispVal -> T.Text
showVal (Atom name) =
  name
showVal (List xs) =
  T.concat ["(", unwordsList xs, ")"]
showVal (DottedList xs x) =
  T.concat ["(", unwordsList xs, " . ", showVal x, ")"]
showVal (Vector xs) =
  T.concat ["#(", T.pack (show xs), ")"]
showVal (Number x) =
  T.pack (show x)
showVal (Float x) =
  T.pack (show x)
showVal (Ratio x) =
  T.concat [T.pack (show (numerator x)), "/", T.pack (show (denominator x))]
showVal (Complex x) =
  T.concat [T.pack (show (realPart x)),  ",", T.pack (show (imagPart x)), "i"]
showVal (String xs) =
  T.concat ["\"", xs, "\""]
showVal (Character c) =
  T.concat ["#\\", T.pack [c]]
showVal (Bool True) =
  "#t"
showVal (Bool False) =
  "#f"
showVal (PrimOp _) =
  "<primitive>"
showVal (Lambda _ _) =
  "<lambda>"
showVal (Port _) =
  "<IO port>"
showVal Nil =
  "()"

instance Show LispVal where
  show = T.unpack . showVal

showError :: LispError -> T.Text
showError (NumArgs expected found) =
  T.concat ["Expected ", T.pack (show expected), " args: found values ", unwordsList found]
showError (TypeMismatch expected found) =
  T.concat ["Invalid type: expected ", expected, " value, found ", showVal found]
showError (Syntax parseErr) =
  T.concat ["Parse error at ", T.pack (show parseErr)]
showError (BadSpecialForm message form) =
  T.concat [message, ": ", T.pack (show form)]
showError (NotFunction func) =
  T.concat ["Not a function: ", T.pack (show func)]
showError (UnboundVar varname) =
  T.concat ["Unbound variable: ", varname]
showError (Default message) =
  T.concat ["Default error: ", message]

instance Show LispError where
  show = T.unpack . showError
