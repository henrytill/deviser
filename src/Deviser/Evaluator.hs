{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Deviser.Evaluator where

import Data.Monoid ((<>))
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as T
import Deviser.Types

-- List Primitives

cons :: MonadError LispError m => [LispVal] -> m LispVal
cons [h, List []]         = return (List [h])
cons [h, List xs]         = return (List (h : xs))
cons [h, DottedList xs x] = return (DottedList (h : xs) x)
cons [h, x]               = return (DottedList [h] x)
cons h                    = throwError (NumArgs 2 h)

car :: MonadError LispError m => [LispVal] -> m LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [x]                    = throwError (TypeMismatch "pair" x)
car x                      = throwError (NumArgs 1 x)

cdr :: MonadError LispError m => [LispVal] -> m LispVal
cdr [List (_ : xs)]             = return (List xs)
cdr [DottedList (_ : s : xs) x] = return (DottedList (s : xs) x)
cdr [DottedList [_] x]          = return x
cdr [x]                         = throwError (TypeMismatch "pair" x)
cdr x                           = throwError (NumArgs 1 x)

eqv :: (Applicative f, Eq a) => a -> a -> f LispVal
eqv x y = Bool <$> pure (x == y)

-- Unary Operations

symbolp :: MonadError LispError m => LispVal -> m LispVal
symbolp (Atom _)  = return (Bool True)
symbolp _         = return (Bool False)

numberp :: MonadError LispError m => LispVal -> m LispVal
numberp (Number _) = return (Bool True)
numberp _          = return (Bool False)

stringp :: MonadError LispError m => LispVal -> m LispVal
stringp (String _) = return (Bool True)
stringp _          = return (Bool False)

boolp :: MonadError LispError m => LispVal -> m LispVal
boolp (Bool _) = return (Bool True)
boolp _        = return (Bool False)

listp :: MonadError LispError m => LispVal -> m LispVal
listp (List _)         = return (Bool True)
listp (DottedList _ _) = return (Bool True)
listp _                = return (Bool False)

symbolToString :: MonadError LispError m => LispVal -> m LispVal
symbolToString (Atom s) = return (String s)
symbolToString _        = return (String "")

stringToSymbol :: MonadError LispError m => LispVal -> m LispVal
stringToSymbol (String s) = return (Atom s)
stringToSymbol _          = return (Atom "")

-- Evaluator

-- type UnaryOp  = LispVal -> Eval LispVal
-- type BinaryOp = LispVal -> LispVal -> Eval LispVal

unaryOp :: MonadError LispError m => (LispVal -> m a) -> [LispVal] -> m a
unaryOp op [x] = op x
unaryOp _  xs  = throwError (NumArgs 1 xs)

binaryOp :: MonadError LispError m => (LispVal -> LispVal -> m a) -> [LispVal] -> m a
binaryOp op [x, y] = op x y
binaryOp _  xs     = throwError (NumArgs 1 xs)

binaryOpFold
  :: MonadError LispError m
  => (LispVal -> LispVal -> m LispVal)
  -> LispVal
  -> [LispVal]
  -> m LispVal
binaryOpFold op _    [a,b]   = op a b
binaryOpFold _  _    args@[] = throwError (NumArgs 2 args)
binaryOpFold op farg args    = foldM op farg args

numericBinOp
  :: MonadError LispError m
  => (Integer -> Integer -> Integer)
  -> LispVal
  -> LispVal
  -> m LispVal
numericBinOp op (Number x) (Number y) = return (Number (op x  y))
numericBinOp _  Nil        (Number y) = return (Number y)
numericBinOp _  (Number x) Nil        = return (Number x)
numericBinOp _  x          (Number _) = throwError (TypeMismatch "number" x)
numericBinOp _  (Number _) y          = throwError (TypeMismatch "number" y)
numericBinOp _  x          _          = throwError (TypeMismatch "number" x)

numBoolBinOp
  :: MonadError LispError m
  => (Integer -> Integer -> Bool)
  -> LispVal
  -> LispVal
  -> m LispVal
numBoolBinOp op (Number x) (Number y) = return (Bool (op x  y))
numBoolBinOp _  x          (Number _) = throwError (TypeMismatch "number" x)
numBoolBinOp _  (Number _) y          = throwError (TypeMismatch "number" y)
numBoolBinOp _  x          _          = throwError (TypeMismatch "number" x)

eqOp
  :: MonadError LispError m
  => (Bool -> Bool -> Bool)
  -> LispVal
  -> LispVal
  -> m LispVal
eqOp op (Bool x) (Bool y) = return (Bool (op x y))
eqOp _  x        (Bool _) = throwError (TypeMismatch "bool" x)
eqOp _  (Bool _) y        = throwError (TypeMismatch "bool" y)
eqOp _  x        _        = throwError (TypeMismatch "bool" x)

primEnv :: EnvCtx
primEnv = Map.fromList
  [ ("cons",           PrimOp cons)
  , ("car",            PrimOp car)
  , ("cdr",            PrimOp cdr)
  , ("eq?",            PrimOp (binaryOp     eqv))
  , ("+",              PrimOp (binaryOpFold (numericBinOp (+)) (Number 0)))
  , ("*",              PrimOp (binaryOpFold (numericBinOp (*)) (Number 1)))
  , ("-",              PrimOp (binaryOp     (numericBinOp (-))))
  , ("/",              PrimOp (binaryOp     (numericBinOp div)))
  , ("mod",            PrimOp (binaryOp     (numericBinOp mod)))
  , ("quotient",       PrimOp (binaryOp     (numericBinOp quot)))
  , ("remainder",      PrimOp (binaryOp     (numericBinOp rem)))
  , ("symbol?",        PrimOp (unaryOp      symbolp))
  , ("number?",        PrimOp (unaryOp      numberp))
  , ("string?",        PrimOp (unaryOp      stringp))
  , ("bool?",          PrimOp (unaryOp      boolp))
  , ("list?",          PrimOp (unaryOp      listp))
  , ("symbol->string", PrimOp (unaryOp      symbolToString))
  , ("string->symbol", PrimOp (unaryOp      stringToSymbol))
  , ("=",              PrimOp (binaryOp     (numBoolBinOp (==))))
  , ("<",              PrimOp (binaryOp     (numBoolBinOp (<))))
  , (">",              PrimOp (binaryOp     (numBoolBinOp (>))))
  , ("/=",             PrimOp (binaryOp     (numBoolBinOp (/=))))
  , (">=",             PrimOp (binaryOp     (numBoolBinOp (>=))))
  , ("<=",             PrimOp (binaryOp     (numBoolBinOp (<=))))
  , ("&&",             PrimOp (binaryOpFold (eqOp (&&)) (Bool True)))
  , ("||",             PrimOp (binaryOpFold (eqOp (||)) (Bool False)))
  ]

getVar :: (MonadReader EnvCtx m, MonadError LispError m) => LispVal -> m LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throwError (UnboundVar atom)
getVar n = throwError (TypeMismatch "atom" n)

ifExpr :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> LispVal -> LispVal -> m LispVal
ifExpr predicate consequent alternate = do
  ifResult <- eval predicate
  case ifResult of
    (Bool True)  -> eval consequent
    (Bool False) -> eval alternate
    x            -> throwError (TypeMismatch "bool" x)


condExp :: (MonadError LispError m, MonadReader EnvCtx m) => [LispVal] -> m LispVal
condExp [List [Atom "else", consequent]] =
  eval consequent
condExp (List [predicate, consequent] : xs) = do
  predResult <- eval predicate
  case predResult of
    (Bool True)  -> eval consequent
    (Bool False) -> condExp xs
    x            -> throwError (TypeMismatch "bool" x)
condExp x =
  throwError (NumArgs 1 x)

eqf :: MonadError LispError m => LispVal -> LispVal -> m Bool
eqf x y = do
  res <- eqv y x
  case res of
    Bool b -> return b
    _      -> throwError (TypeMismatch "bool" x)

caseExpr :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> [LispVal] -> m LispVal
caseExpr _ x@[] =
  throwError (NumArgs 1 x)
caseExpr _ (List (Atom "else" : thenBody) : _) =
  last <$> mapM eval thenBody
caseExpr valExpr (List (List datums : thenBody) : clauses) = do
  result     <- eval valExpr
  foundMatch <- or <$> traverse (eqf result) datums
  if foundMatch
    then last <$> mapM eval thenBody
    else caseExpr valExpr clauses
caseExpr valExpr clauses =
  throwError (BadSpecialForm "Ill-constructed case expression" (List (Atom "case" : valExpr : clauses)))

letBindingsAreValid :: [LispVal] -> Bool
letBindingsAreValid = all folder
  where
    folder (List [Atom _, _]) = True
    folder _                  = False

collectLetBindings :: [LispVal] -> Map.Map T.Text LispVal
collectLetBindings = foldl folder (Map.fromList [])
  where
    folder acc (List [Atom var, expr]) = Map.insert var expr acc
    folder _   _                       = Map.fromList []

letExpr :: (MonadError LispError m, MonadReader EnvCtx m) => [LispVal] -> [LispVal] -> m LispVal
letExpr pairs exprs =
  if letBindingsAreValid pairs
    then do
    bindings <- traverse eval (collectLetBindings pairs)
    local (mappend bindings) (beginExpr exprs)
    else throwError (BadSpecialForm "Ill-formed let-expression" (List pairs))

ensureAtom :: MonadError LispError m => LispVal -> m LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n          = throwError (TypeMismatch "atom" n)

extractVar :: MonadError LispError m => LispVal -> m T.Text
extractVar (Atom atom) = return atom
extractVar n           = throwError (TypeMismatch "atom" n)

defExpr :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> LispVal -> m LispVal
defExpr var expr = do
  evaledExpr   <- eval expr
  extractedVar <- extractVar <$> ensureAtom var
  insertMe     <- extractedVar
  local (Map.insert insertMe evaledExpr) (return var)

beginExpr :: (MonadError LispError m, MonadReader EnvCtx m) => [LispVal] -> m LispVal
beginExpr [List (Atom "define" : [Atom var, expr]), rest] = do
  evaledExpr <- eval expr
  local (Map.insert var evaledExpr) (eval rest)
beginExpr (List (Atom "define" : [Atom var, expr]) : rest) = do
  evaledExpr <- eval expr
  local (Map.insert var evaledExpr) (beginExpr rest)
beginExpr [x] =
  eval x
beginExpr (x:xs) =
  eval x >> beginExpr xs
beginExpr [] =
  return Nil

lambdaExpr
  :: (MonadReader EnvCtx m, MonadError LispError m)
  => [LispVal]
  -> LispVal
  -> m LispVal
lambdaExpr params expr = do
  env             <- ask
  unwrappedParams <- mapM extractVar params
  return (Lambda unwrappedParams Nothing expr env)

lambdaExprVarargs
  :: (MonadReader EnvCtx m, MonadError LispError m)
  => LispVal
  -> LispVal
  -> m LispVal
lambdaExprVarargs (Atom p) expr = do
  env             <- ask
  return (Lambda [] (Just p) expr env)
lambdaExprVarargs _        _          =
  throwError (BadSpecialForm "vararg" Nil)

apply
  :: (MonadError LispError m, MonadReader EnvCtx m)
  => LispVal
  -> [LispVal]
  -> m LispVal
apply f args = do
  funVar     <- eval f
  evaledArgs <- mapM eval args
  case funVar of
    (PrimOp internalFn)                     -> either throwError pure (internalFn evaledArgs)
    (Lambda params Nothing   body boundEnv) -> local (mappend (Map.fromList (zip params evaledArgs) <> boundEnv)) (eval body)
    (Lambda []     (Just vp) body boundEnv) -> local (mappend (Map.insert vp (List evaledArgs) boundEnv))         (eval body)
    _                                       -> throwError (NotFunction funVar)

evalExpr :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> m LispVal
evalExpr expr = do
  e <- eval expr
  case e of
    v@(List _) -> eval v
    _          -> return e

eval
  :: (MonadError LispError m, MonadReader EnvCtx m)
  => LispVal
  -> m LispVal
eval v@(String _)                                 = return v
eval v@(Number _)                                 = return v
eval v@(Bool _)                                   = return v
eval   (List (Atom "list" : xs))                  = List <$> pure xs
eval   (List [Atom "quote", value])               = return value
eval   (List [])                                  = return Nil
eval   Nil                                        = return Nil
eval v@(Atom _)                                   = getVar v
eval   (List [Atom "if", predicate, conseq, alt]) = ifExpr predicate conseq alt
eval   (List (Atom "cond" : clauses))             = condExp clauses
eval   (List (Atom "case" : key : clauses))       = caseExpr key clauses
eval   (List (Atom "let" : List pairs : exprs))   = letExpr pairs exprs
eval   (List (Atom "begin" : rest))               = beginExpr rest
eval   (List [Atom "define", varExpr, expr])      = defExpr varExpr expr
eval   (List [Atom "lambda", List params, expr])  = lambdaExpr params expr
eval   (List [Atom "lambda", vs@(Atom _), expr])  = lambdaExprVarargs vs expr
eval   (List [Atom "eval", value])                = evalExpr value
eval   (List (f : args))                          = apply f args
eval   badForm                                    = throwError (BadSpecialForm "Unrecognized special form" badForm)

expandAndSplice :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> m LispVal -> m LispVal
expandAndSplice (List [Atom "unquote-splicing", xs@(List _)]) acc = do
  b <- acc
  a <- eval xs
  case (a, b) of
    (List as, List bs) -> List <$> pure (as ++ bs)
    (h,       t)       -> cons [h, t]
expandAndSplice a acc = do
  t <- acc
  h <- expandQuasiquoted a
  cons [h, t]

expandQuasiquoted :: (MonadError LispError m, MonadReader EnvCtx m) => LispVal -> m LispVal
expandQuasiquoted (List [Atom "unquote", x@(Atom _)])  = getVar x *> pure x
expandQuasiquoted (List [Atom "unquote", xs@(List _)]) = eval xs
expandQuasiquoted (List xs)                            = foldr expandAndSplice (pure (List [])) xs
expandQuasiquoted x                                    = pure x

quote :: LispVal -> LispVal
quote x = List [Atom "quote", x]

expand :: (MonadReader EnvCtx m, MonadError LispError m) => LispVal -> m LispVal
expand (List [Atom "quasiquote", value]) = quote <$> expandQuasiquoted value
expand (List xs)                         = List <$> traverse expand xs
expand x                                 = pure x
