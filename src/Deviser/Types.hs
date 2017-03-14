{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Deviser.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Complex
import Data.Ratio (numerator, denominator)
import Text.Parsec (ParseError)
import qualified Data.Map as Map
import qualified Data.Text as T

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

newtype Eval a = Eval { unEval :: StateT EnvCtx (ReaderT EnvCtx (ExceptT LispError IO)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError LispError
           , MonadIO
           , MonadReader EnvCtx
           , MonadState EnvCtx
           )

runUnEval :: r -> s -> StateT s (ReaderT r (ExceptT e m)) a -> m (Either e (a, s))
runUnEval r s action = runExceptT (runReaderT (runStateT action s) r)

type EnvCtx = Map.Map T.Text LispVal

type ThrowsError   = Either LispError
type IOThrowsError = ExceptT LispError IO

-- | A 'LispVal' is a Lisp value
data LispVal where
  Atom       :: T.Text -> LispVal
  List       :: [LispVal] -> LispVal
  DottedList :: [LispVal] -> LispVal -> LispVal
  Vector     :: Array Int LispVal -> LispVal
  Number     :: Integer -> LispVal
  Float      :: Double -> LispVal
  Ratio      :: Rational -> LispVal
  Complex    :: Complex Double -> LispVal
  String     :: T.Text -> LispVal
  Character  :: Char -> LispVal
  Bool       :: Bool -> LispVal
  PrimOp     :: ([LispVal] -> ThrowsError LispVal) -> LispVal
  Lambda     :: { lambdaArgs    :: [T.Text]
                , lambdaVarargs :: Maybe T.Text
                , lambdaBody    :: LispVal
                , lambdaEnv     :: EnvCtx
                } -> LispVal
  Nil        :: LispVal

instance Eq LispVal where
  Atom x          == Atom y          = x == y
  List x          == List y          = x == y
  DottedList xs x == DottedList ys y = xs == ys && x == y
  Vector x        == Vector y        = x == y
  Number x        == Number y        = x == y
  Float x         == Float y         = x == y
  Ratio x         == Ratio y         = x == y
  Complex x       == Complex y       = x == y
  String x        == String y        = x == y
  Character x     == Character y     = x == y
  Bool x          == Bool y          = x == y
  Nil             == Nil             = True
  _               == _               = False

data LispError where
  NumArgs        :: Integer -> [LispVal] -> LispError
  TypeMismatch   :: T.Text -> LispVal -> LispError
  Syntax         :: ParseError -> LispError
  BadSpecialForm :: T.Text -> LispVal -> LispError
  NotFunction    :: LispVal -> LispError
  UnboundVar     :: T.Text -> LispError
  Default        :: T.Text -> LispError

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
showVal Lambda{} =
  "<lambda>"
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
  T.concat ["BadSpecialForm: ", message, ", ", T.pack (show form)]
showError (NotFunction func) =
  T.concat ["Not a function: ", T.pack (show func)]
showError (UnboundVar varname) =
  T.concat ["Unbound variable: ", varname]
showError (Default message) =
  T.concat ["Default error: ", message]

instance Show LispError where
  show = T.unpack . showError
