module Deviser.Types where

import Control.Monad.Except (ExceptT)
import Data.Array
import Data.Complex
import Data.Ratio (numerator, denominator)
import Data.IORef
import System.IO (Handle)
import Text.ParserCombinators.Parsec (ParseError)

data LispVal =
    Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    | Number Integer
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    | String String
    | Character Char
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { funcParams  :: [String]
           , funcVarargs :: Maybe String
           , funcBody    :: [LispVal]
           , funcClosure :: Env
           }
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

data LispError =
    NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Syntax ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO


-- Show

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom name)       = name
showVal (List xs)         = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
showVal (Vector xs)       = "#(" ++ show xs ++ ")"
showVal (Number x)        = show x
showVal (Float x)         = show x
showVal (Ratio x)         = show (numerator x) ++ "/" ++ show (denominator x)
showVal (Complex x)       = show (realPart x) ++ "+" ++ show (imagPart x) ++ "i"
showVal (String xs)       = "\"" ++ xs ++ "\""
showVal (Character c)     = "#\\" ++ [c]
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func ps vs _ _)  = "(lambda ("
    ++ unwords (map show ps)
    ++ (case vs of
          Nothing  -> ""
          Just arg -> " . " ++ arg)
    ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

instance Show LispVal where
    show = showVal

showError :: LispError -> String
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " value, found " ++ show found
showError (Syntax parseErr)             = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (Default message)             = "Default error: " ++ message

instance Show LispError where
    show = showError
