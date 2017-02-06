{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Deviser.Types where

import Control.Monad.Except (ExceptT)
import Data.Array
import Data.Complex
import Data.Ratio (numerator, denominator)
import qualified Data.Text as T
import Data.Typeable
import Data.IORef
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
  | PrimOp ([LispVal] -> ThrowsError LispVal)
  | Lambda { funcParams  :: [T.Text]
           , funcVarargs :: Maybe T.Text
           , funcBody    :: [LispVal]
           , funcClosure :: Env
           }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  deriving Typeable

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Syntax ParseError
  | BadSpecialForm T.Text LispVal
  | NotFunction T.Text T.Text
  | UnboundVar T.Text T.Text
  | Default T.Text

type Env = IORef [(T.Text, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO


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
showVal (Lambda ps vs _ _)  =
  T.concat ["(lambda ("
           , T.unwords ps
           , (case vs of
                Nothing  -> ""
                Just arg -> T.concat [" . ", arg])
           , ") ...)"
           ]
showVal (Port _)   =
  "<IO port>"
showVal (IOFunc _) =
  "<IO primitive>"

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
showError (NotFunction message func) =
  T.concat [message, ": ", T.pack (show func)]
showError (UnboundVar message varname) =
  T.concat [message, ": ", varname]
showError (Default message) =
  T.concat ["Default error: ", message]

instance Show LispError where
  show = T.unpack . showError
