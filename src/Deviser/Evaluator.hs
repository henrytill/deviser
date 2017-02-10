{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Deviser.Evaluator where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Deviser.Types

type UnaryOp = LispVal -> Eval LispVal
type BinaryOp = LispVal -> LispVal -> Eval LispVal


-- Environment Handling

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throwError (UnboundVar atom)
getVar n = throwError (TypeMismatch "Not a variable: " n)


-- List Primitives

car :: [LispVal] -> Eval LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [x]                    = throwError (TypeMismatch "pair" x)
car x                      = throwError (NumArgs 1 x)

cdr :: [LispVal] -> Eval LispVal
cdr [List (_ : xs)]             = return (List xs)
cdr [DottedList (_ : s : xs) x] = return (DottedList (s : xs) x)
cdr [DottedList [_] x]          = return x
cdr [x]                         = throwError (TypeMismatch "pair" x)
cdr x                           = throwError (NumArgs 1 x)

cons :: [LispVal] -> Eval LispVal
cons [h, List []]         = return (List [h])
cons [h, List xs]         = return (List (h : xs))
cons [h, DottedList xs x] = return (DottedList (h : xs) x)
cons [h, x]               = return (DottedList [h] x)
cons h                    = throwError (NumArgs 2 h)

eqList :: (LispVal -> LispVal -> Eval LispVal) -> [LispVal] -> [LispVal] -> Eval LispVal
eqList eqFunc (x:xs) (y:ys) = do
  (Bool these) <- eqv x y
  (Bool those) <- eqList eqFunc xs ys
  return (Bool (these && those))
eqList _ _ _ = error "eqList: this should never happen!"

eqv :: LispVal -> LispVal -> Eval LispVal
eqv (Bool x)          (Bool y)          = return (Bool (x == y))
eqv (Number x)        (Number y)        = return (Bool (x == y))
eqv (Float x)         (Float y)         = return (Bool (x == y))
eqv (Ratio x)         (Ratio y)         = return (Bool (x == y))
eqv (Complex x)       (Complex y)       = return (Bool (x == y))
eqv (String x)        (String y)        = return (Bool (x == y))
eqv (Atom x)          (Atom y)          = return (Bool (x == y))
eqv (List l1 @ _)     (List l2 @ _)     = eqList eqv l1 l2
eqv (DottedList xs x) (DottedList ys y) = eqList eqv (xs ++ [x]) (ys ++ [y])
eqv _ _                                 = return (Bool False)


-- Unary Operations

symbolp :: LispVal -> Eval LispVal
symbolp (Atom _)  = return (Bool True)
symbolp _         = return (Bool False)

numberp :: LispVal -> Eval LispVal
numberp (Number _) = return (Bool True)
numberp _          = return (Bool False)

stringp :: LispVal -> Eval LispVal
stringp (String _) = return (Bool True)
stringp _          = return (Bool False)

boolp :: LispVal -> Eval LispVal
boolp (Bool _) = return (Bool True)
boolp _        = return (Bool False)

listp :: LispVal -> Eval LispVal
listp (List _)         = return (Bool True)
listp (DottedList _ _) = return (Bool True)
listp _                = return (Bool False)

symbolToString :: LispVal -> Eval LispVal
symbolToString (Atom s) = return (String s)
symbolToString _        = return (String "")

stringToSymbol :: LispVal -> Eval LispVal
stringToSymbol (String s) = return (Atom s)
stringToSymbol _          = return (Atom "")


-- IO Primitives

-- applyProc :: [LispVal] -> IOThrowsError LispVal
-- applyProc [func, List args] = apply func args
-- applyProc (func : args)     = apply func args
-- applyProc _                 = throwError (Default "applyProc: bad arguments")

-- makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
-- makePort mode [String filename] = fmap Port (liftIO (openFile (T.unpack filename) mode))
-- makePort _ _                    = throwError (Default "makePort: bad arguments")

-- closePort :: [LispVal] -> IOThrowsError LispVal
-- closePort [Port port] = liftIO (hClose port >> return (Bool True))
-- closePort _           = return (Bool False)

-- readProc :: [LispVal] -> IOThrowsError LispVal
-- readProc []          = readProc [Port stdin]
-- readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr . T.pack
-- readProc _           = throwError (Default "readProc: bad arguments")

-- writeProc :: [LispVal] -> IOThrowsError LispVal
-- writeProc [obj]            = writeProc [obj, Port stdout]
-- writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
-- writeProc _                = throwError (Default "writeProc: bad arguments")

-- readContents :: [LispVal] -> IOThrowsError LispVal
-- readContents [String filename] = fmap (String . T.pack) (liftIO (readFile (T.unpack filename)))
-- readContents _                 = throwError (Default "readContents: bad arguments")

-- load :: T.Text -> IOThrowsError [LispVal]
-- load filename = liftIO (readFile (T.unpack filename)) >>= liftThrows . readExprList . T.pack

-- readAll :: [LispVal] -> IOThrowsError LispVal
-- readAll [String filename] = fmap List (load filename)
-- readAll _                 = throwError (Default "readAll: bad arguments")


-- Evaluator

unaryOp :: UnaryOp -> [LispVal] -> Eval LispVal
unaryOp op [x] = op x
unaryOp _  xs  = throwError (NumArgs 1 xs)

binaryOp :: BinaryOp -> [LispVal] -> Eval LispVal
binaryOp op [x, y] = op x y
binaryOp _  xs     = throwError (NumArgs 1 xs)

binaryOpFold :: BinaryOp -> LispVal -> [LispVal] -> Eval LispVal
binaryOpFold op _    [a,b]     = op a b
binaryOpFold _  _    args @ [] = throwError (NumArgs 2 args)
binaryOpFold op farg args      = foldM op farg args

numericBinOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numericBinOp op (Number x) (Number y) = return (Number (op x  y))
numericBinOp _  Nil        (Number y) = return (Number y)
numericBinOp _  (Number x) Nil        = return (Number x)
numericBinOp _  x          (Number _) = throwError (TypeMismatch "numeric op " x)
numericBinOp _  (Number _) y          = throwError (TypeMismatch "numeric op " y)
numericBinOp _  x          _          = throwError (TypeMismatch "numeric op " x)

numBoolBinOp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numBoolBinOp op (Number x) (Number y) = return (Bool (op x  y))
numBoolBinOp _  x          (Number _) = throwError (TypeMismatch "numeric op " x)
numBoolBinOp _  (Number _) y          = throwError (TypeMismatch "numeric op " y)
numBoolBinOp _  x          _          = throwError (TypeMismatch "numeric op " x)

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return (Bool (op x y))
eqOp _  x        (Bool _) = throwError (TypeMismatch "bool op " x)
eqOp _  (Bool _) y        = throwError (TypeMismatch "bool op " y)
eqOp _  x        _        = throwError (TypeMismatch "bool op " x)

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = PrimOp . IFunc

primEnv :: [(T.Text, LispVal)]
primEnv =
  [ ("cons",           mkF cons)
  , ("car",            mkF car)
  , ("cdr",            mkF cdr)
  , ("eq?",            mkF (binaryOp     eqv))
  , ("+",              mkF (binaryOpFold (numericBinOp (+)) (Number 0)))
  , ("*",              mkF (binaryOpFold (numericBinOp (*)) (Number 1)))
  , ("-",              mkF (binaryOp     (numericBinOp (-))))
  , ("/",              mkF (binaryOp     (numericBinOp div)))
  , ("mod",            mkF (binaryOp     (numericBinOp mod)))
  , ("quotient",       mkF (binaryOp     (numericBinOp quot)))
  , ("remainder",      mkF (binaryOp     (numericBinOp rem)))
  , ("symbol?",        mkF (unaryOp      symbolp))
  , ("number?",        mkF (unaryOp      numberp))
  , ("string?",        mkF (unaryOp      stringp))
  , ("bool?",          mkF (unaryOp      boolp))
  , ("list?",          mkF (unaryOp      listp))
  , ("symbol->string", mkF (unaryOp      symbolToString))
  , ("string->symbol", mkF (unaryOp      stringToSymbol))
  , ("=",              mkF (binaryOp     (numBoolBinOp (==))))
  , ("<",              mkF (binaryOp     (numBoolBinOp (<))))
  , (">",              mkF (binaryOp     (numBoolBinOp (>))))
  , ("/=",             mkF (binaryOp     (numBoolBinOp (/=))))
  , (">=",             mkF (binaryOp     (numBoolBinOp (>=))))
  , ("<=",             mkF (binaryOp     (numBoolBinOp (<=))))
  , ("&&",             mkF (binaryOpFold (eqOp (&&)) (Bool True)))
  , ("||",             mkF (binaryOpFold (eqOp (||)) (Bool False)))
  -- , ("string=?",       strBoolBinOp (==))
  -- , ("string<?",       strBoolBinOp (<))
  -- , ("string>?",       strBoolBinOp (>))
  -- , ("string<=?",      strBoolBinOp (<=))
  -- , ("string>=?",      strBoolBinOp (>=))
  ]


-- ioPrimitives :: [(T.Text, [LispVal] -> IOThrowsError LispVal)]
-- ioPrimitives =
--   [ ("apply",             applyProc)
--   , ("open-input-file",   makePort ReadMode)
--   , ("open-output-file",  makePort WriteMode)
--   , ("close-input-port",  closePort)
--   , ("close-output-port", closePort)
--   , ("read",              readProc)
--   , ("write",             writeProc)
--   , ("read-contents",     readContents)
--   , ("read-all",          readAll)
--   ]

-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= flip bindVars allPrimitives
--   where
--     makePrimOp ctor (var, func) = (var, ctor func)
--     allPrimitives = map (makePrimOp PrimOp) primitives ++ map (makePrimOp IOPrimOp) ioPrimitives

-- makeFunc :: Monad m => Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> m LispVal
-- makeFunc vs env ps b = return (Lambda (map showVal ps) vs b env)

-- makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
-- makeNormalFunc = makeFunc Nothing

-- makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
-- makeVarargs = makeFunc . Just . showVal

-- apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
-- apply (PrimOp f)                           args = liftThrows (f args)
-- apply (Lambda params varargs body closure) args =
--   if num params /= num args && isNothing varargs
--   then throwError (NumArgs (num params) args)
--   else liftIO (bindVars closure (zip params args)) >>= bindVarArgs varargs >>= evalBody
--   where
--     num                            = toInteger . length
--     remainingArgs                  = drop (length params) args
--     bindVarArgs (Just argName) env = liftIO (bindVars env [(argName, List remainingArgs)])
--     bindVarArgs Nothing        env = return env
--     evalBody env                   = fmap last (mapM (eval env) body)
-- apply x _ = throwError (TypeMismatch "func" x)

ifExpr ::  LispVal -> LispVal -> LispVal -> Eval LispVal
ifExpr predicate consequent alternate = do
  ifResult <- eval predicate
  case ifResult of
    (Bool True)  -> eval consequent
    (Bool False) -> eval alternate
    x            -> throwError (TypeMismatch "bool" x)

condExp :: [LispVal] -> Eval LispVal
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

eqf :: LispVal -> LispVal -> Eval Bool
eqf x y = do
  res <- eqv y x
  case res of
    Bool b -> return b
    _      -> throwError (TypeMismatch "bool" x)

caseExpr :: LispVal -> [LispVal] -> Eval LispVal
caseExpr _ x @ [] =
  throwError (NumArgs 1 x)
caseExpr _ (List (Atom "else" : thenBody) : _) =
  fmap last (mapM eval thenBody)
caseExpr valExpr (List (List datums : thenBody) : clauses) = do
  result     <- eval valExpr
  foundMatch <- fmap or (traverse (eqf result) datums)
  if foundMatch
    then fmap last (mapM eval thenBody)
    else caseExpr valExpr clauses
caseExpr valExpr clauses =
  throwError (BadSpecialForm "Ill-constructed case expression" (List (Atom "case" : valExpr : clauses)))

letBindingsAreValid :: [LispVal] -> Bool
letBindingsAreValid = Prelude.all folder
  where
    folder (List [Atom _, _]) = True
    folder _                  = False

collectLetBindings :: [LispVal] -> EnvCtx
collectLetBindings = Prelude.foldl folder (Map.fromList [])
  where
    folder acc (List [Atom var, expr]) = Map.insert var expr acc
    folder _   _                       = Map.fromList []

letExpr :: [LispVal] -> [LispVal] -> Eval LispVal
letExpr pairs exprs =
  if letBindingsAreValid pairs
    then do
    bindings <- traverse eval (collectLetBindings pairs)
    env      <- ask
    local (const (bindings <> env)) (evalBody exprs)
    else throwError (BadSpecialForm "Ill-formed let-expression" (List pairs))

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n @ (Atom _) = return n
ensureAtom n            = throwError (TypeMismatch "expected an atom" n)

extractVar :: LispVal -> Eval T.Text
extractVar (Atom atom) = return atom
extractVar n           = throwError (TypeMismatch "expected an atom" n)

defExpr :: LispVal -> LispVal -> Eval LispVal
defExpr var expr = do
  evaledExpr   <- eval expr
  env          <- ask
  extractedVar <- fmap extractVar (ensureAtom var)
  insertMe     <- extractedVar
  let envFn = const (Map.insert insertMe evaledExpr env)
  local envFn (return var)

evalBody :: [LispVal] -> Eval LispVal
evalBody [List (Atom "define" : [Atom var, expr]), rest] = do
  evaledExpr <- eval expr
  env        <- ask
  local (const (Map.insert var evaledExpr env)) (eval rest)
evalBody (List (Atom "define" : [Atom var, expr]) : rest) = do
  evaledExpr <- eval expr
  env        <- ask
  local (const (Map.insert var evaledExpr env)) (evalBody rest)
evalBody [x] =
  eval x
evalBody (x:xs) =
  eval x >> evalBody xs
evalBody [] =
  return Nil

lambdaExpr :: [LispVal] -> LispVal -> Eval LispVal
lambdaExpr params expr = do
  envLocal <- ask
  return (Lambda (IFunc (applyLambda expr params)) envLocal)

lambdaExprVarargs :: LispVal -> LispVal -> Eval LispVal
lambdaExprVarargs p expr = do
  envLocal <- ask
  return (Lambda (IFunc (applyLambdaVarargs expr p)) envLocal)

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env             <- ask
  evaledArgs      <- mapM eval args
  extractedParams <- mapM extractVar params
  let envFn = const (Map.fromList (zip extractedParams evaledArgs)  <> env)
  local envFn (eval expr)

applyLambdaVarargs :: LispVal -> LispVal -> [LispVal] -> Eval LispVal
applyLambdaVarargs expr (Atom p) args = do
  env        <- ask
  evaledArgs <- mapM eval args
  let envFn = const (Map.insert p (List evaledArgs) env)
  local envFn (eval expr)
applyLambdaVarargs _ _ _ = throwError (BadSpecialForm "vararg" Nil)

apply :: LispVal -> [LispVal] -> Eval LispVal
apply f args = do
  funVar     <- eval f
  evaledArgs <- mapM eval args
  case funVar of
    (PrimOp (IFunc internalFn))          -> internalFn evaledArgs
    (Lambda (IFunc internalFn) boundEnv) -> local (const boundEnv) (internalFn evaledArgs)
    _                                    -> throwError (NotFunction funVar)

evaler :: LispVal -> Eval LispVal
evaler expr = do
  e <- eval expr
  case e of
    v @ (List _) -> eval v
    _            -> return e

eval :: LispVal -> Eval LispVal
eval value @ (String _)                               = return value
eval value @ (Number _)                               = return value
eval value @ (Bool _)                                 = return value
eval (List [Atom "quote", value])                     = return value
eval (List [])                                        = return Nil
eval Nil                                              = return Nil
eval a @ (Atom _)                                     = getVar a
eval (List [Atom "if", predicate, conseq, alt])       = ifExpr predicate conseq alt
eval (List (Atom "cond" : clauses))                   = condExp clauses
eval (List (Atom "case" : key : clauses))             = caseExpr key clauses
eval (List (Atom "let" : List pairs : exprs))         = letExpr pairs exprs
eval (List (Atom "begin" : rest))                     = evalBody rest
eval (List [Atom "define", varExpr, expr])            = defExpr varExpr expr
eval (List [Atom "lambda", List params, expr])        = lambdaExpr params expr
eval (List [Atom "lambda", vs @ (Atom _), expr])      = lambdaExprVarargs vs expr
eval (List [Atom "eval", value])                      = evaler value
eval (List (f : args))                                = apply f args
eval badForm                                          = throwError (BadSpecialForm "Unrecognized special form" badForm)

-- eval (List [Atom "set!", Atom var, form])                                   = eval env form >>= setVar env var
-- eval (List [Atom "define", Atom var, form])                                 = eval env form >>= defineVar env var
-- eval (List (Atom "define" : List (Atom var : params) : body))               = makeNormalFunc env params body >>= defineVar env var
-- eval (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
-- eval (List (Atom "lambda" : DottedList params varargs : body))              = makeVarargs varargs env params body 
-- eval (List (Atom "lambda" : varargs @ (Atom _) : body))                     = makeVarargs varargs env [] body
-- eval (List [Atom "load", String filename])                                  = load filename >>= fmap last . mapM (eval env)
