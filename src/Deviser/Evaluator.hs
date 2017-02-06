{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Deviser.Evaluator where

import Control.Monad.Except
import Data.IORef
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import System.IO
import Deviser.Types
import Deviser.Parser (readExpr, readExprList)

-- Error Handling

trapError :: (MonadError LispError m) => m T.Text -> m T.Text
trapError action = catchError action (return . showError)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error ("extractValue: this should never happen! " ++ show err)


-- Environment Handling

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrowsError :: IOThrowsError T.Text -> IO T.Text
runIOThrowsError action = fmap extractValue (runExceptT (trapError action))

isBound :: Env -> T.Text -> IO Bool
isBound envRef var = fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> T.Text -> IOThrowsError LispVal
getVar envRef var =
  liftIO (readIORef envRef) >>=
  maybe err (liftIO . readIORef) . lookup var
  where
    err = throwError (UnboundVar "Getting an unbound variable" var)

setVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  liftIO (readIORef envRef) >>=
  maybe err (liftIO . flip writeIORef value) . lookup var >>
  return value
  where
    err = throwError (UnboundVar "Setting an unbound variable" var)

defineVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO (isBound envRef var)
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
    valueRef <- newIORef value
    env      <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env -> [(T.Text, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    addBindings :: (T.Text, LispVal) -> IO (T.Text, IORef LispVal)
    addBindings (var, value) = newIORef value >>= \ref -> return (var, ref)
    extendEnv :: [(T.Text, LispVal)] -> [(T.Text, IORef LispVal)] -> IO [(T.Text, IORef LispVal)]
    extendEnv bs env = fmap (++ env) (mapM addBindings bs)


-- Unary Operations

symbolp :: LispVal -> LispVal
symbolp (Atom _)  = Bool True
symbolp _         = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _          = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _          = Bool False

boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _        = Bool False

listp :: LispVal -> LispVal
listp (List _)         = Bool True
listp (DottedList _ _) = Bool True
listp _                = Bool False

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s
symbolToString _        = String ""

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s
stringToSymbol _          = Atom ""


-- List Primitives

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [x]                    = throwError (TypeMismatch "pair" x)
car x                      = throwError (NumArgs 1 x)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]             = return (List xs)
cdr [DottedList (_ : s : xs) x] = return (DottedList (s : xs) x)
cdr [DottedList [_] x]          = return x
cdr [x]                         = throwError (TypeMismatch "pair" x)
cdr x                           = throwError (NumArgs 1 x)

cons :: [LispVal] -> ThrowsError LispVal
cons [h, List []]         = return (List [h])
cons [h, List xs]         = return (List (h : xs))
cons [h, DottedList xs x] = return (DottedList (h : xs) x)
cons [h, x]               = return (DottedList [h] x)
cons h                    = throwError (NumArgs 2 h)

eqPair :: ([LispVal] -> ThrowsError LispVal) -> (LispVal, LispVal) -> Bool
eqPair f (a, b) = case f [a, b] of
                    Left _           -> False
                    Right (Bool val) -> val
                    Right z          -> error ("eqvPair: this should never happen! " ++ show z)

eqList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqList eqFunc [List x, List y] = return (Bool ((length x == length y) && all (eqPair eqFunc) (zip x y)))
eqList _ z                     = error ("eqList: this should never happen! " ++ show z)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y]                   = return (Bool (x == y))
eqv [Number x, Number y]               = return (Bool (x == y))
eqv [Float x, Float y]                 = return (Bool (x == y))
eqv [Ratio x, Ratio y]                 = return (Bool (x == y))
eqv [Complex x, Complex y]             = return (Bool (x == y))
eqv [String x, String y]               = return (Bool (x == y))
eqv [Atom x, Atom y]                   = return (Bool (x == y))
eqv [l1 @ (List _), l2 @ (List _)]     = eqList eqv [l1, l2]
eqv [DottedList xs x, DottedList ys y] = eqList eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [_, _]                             = return (Bool False)
eqv x                                  = throwError (NumArgs 2 x)


-- IO Primitives

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc _                 = throwError (Default "applyProc: bad arguments")

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port (liftIO (openFile (T.unpack filename) mode))
makePort _ _                    = throwError (Default "makePort: bad arguments")

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port >> return (Bool True))
closePort _           = return (Bool False)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr . T.pack
readProc _           = throwError (Default "readProc: bad arguments")

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
writeProc _                = throwError (Default "writeProc: bad arguments")

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap (String . T.pack) (liftIO (readFile (T.unpack filename)))
readContents _                 = throwError (Default "readContents: bad arguments")

load :: T.Text -> IOThrowsError [LispVal]
load filename = liftIO (readFile (T.unpack filename)) >>= liftThrows . readExprList . T.pack

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List (load filename)
readAll _                 = throwError (Default "readAll: bad arguments")


-- Unpackers

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError (TypeMismatch "number" x)

unpackStr :: LispVal -> ThrowsError T.Text
unpackStr (String s) = return s
-- unpackStr (Number n) = return (show n)
-- unpackStr (Bool b)   = return (show b)
unpackStr x          = throwError (TypeMismatch "string" x)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool x        = throwError (TypeMismatch "bool" x)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
    catchError ((==) <$> unpacker a <*> unpacker b) (const (return False))

-- TODO: re-think this implementation in light of the current conservative
-- approach to coercion
equal :: [LispVal] -> ThrowsError LispVal
equal [l1 @ (List _), l2 @ (List _)] = eqList equal [l1, l2]
equal [a, b] =
  let unpackers       = [ AnyUnpacker unpackNum
                        , AnyUnpacker unpackStr
                        , AnyUnpacker unpackBool
                        ]                                               :: [Unpacker]
      primitiveEquals = fmap or (traverse (unpackEquals a b) unpackers) :: ThrowsError Bool
      eqvEquals       = eqv [a, b] >>= unpackBool                       :: ThrowsError Bool
  in Bool <$> ((||) <$> primitiveEquals <*> eqvEquals)
equal x = throwError (NumArgs 2 x)


-- Evaluator

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _  param @ [_] = throwError (NumArgs 2 param)
numericBinOp op params      = fmap (Number . foldl1 op) (mapM unpackNum params)

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return (f v)
unaryOp _ x   = throwError (NumArgs 1 x)

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op [a, b] = Bool <$> (op <$> unpacker a <*> unpacker b)
boolBinOp _ _ args           = throwError (NumArgs 2 args)

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

strBoolBinOp :: (T.Text -> T.Text -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

primitives :: [(T.Text, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+",              numericBinOp (+))
  , ("-",              numericBinOp (-))
  , ("*",              numericBinOp (*))
  , ("/",              numericBinOp div)
  , ("mod",            numericBinOp mod)
  , ("quotient",       numericBinOp quot)
  , ("remainder",      numericBinOp rem)
  , ("symbol?",        unaryOp symbolp)
  , ("number?",        unaryOp numberp)
  , ("string?",        unaryOp stringp)
  , ("bool?",          unaryOp boolp)
  , ("list?",          unaryOp listp)
  , ("symbol->string", unaryOp symbolToString)
  , ("string->symbol", unaryOp stringToSymbol)
  , ("=",              numBoolBinOp (==))
  , ("<",              numBoolBinOp (<))
  , (">",              numBoolBinOp (>))
  , ("/=",             numBoolBinOp (/=))
  , (">=",             numBoolBinOp (>=))
  , ("<=",             numBoolBinOp (<=))
  , ("&&",             boolBoolBinOp (&&))
  , ("||",             boolBoolBinOp (||))
  , ("string=?",       strBoolBinOp (==))
  , ("string<?",       strBoolBinOp (<))
  , ("string>?",       strBoolBinOp (>))
  , ("string<=?",      strBoolBinOp (<=))
  , ("string>=?",      strBoolBinOp (>=))
  , ("cons",           cons)
  , ("car",            car)
  , ("cdr",            cdr)
  , ("eqv?",           eqv)
  , ("eq?",            eqv)
  , ("equal?",         equal)
  ]


ioPrimitives :: [(T.Text, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply",             applyProc)
  , ("open-input-file",   makePort ReadMode)
  , ("open-output-file",  makePort WriteMode)
  , ("close-input-port",  closePort)
  , ("close-output-port", closePort)
  , ("read",              readProc)
  , ("write",             writeProc)
  , ("read-contents",     readContents)
  , ("read-all",          readAll)
  ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars allPrimitives
  where
    makePrimOp ctor (var, func) = (var, ctor func)
    allPrimitives = map (makePrimOp PrimOp) primitives ++ map (makePrimOp IOFunc) ioPrimitives

makeFunc :: Monad m => Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc vs env ps b = return (Lambda (map showVal ps) vs b env)

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimOp f)                           args = liftThrows (f args)
apply (Lambda params varargs body closure) args =
  if num params /= num args && isNothing varargs
  then throwError (NumArgs (num params) args)
  else liftIO (bindVars closure (zip params args)) >>= bindVarArgs varargs >>= evalBody
  where
    num                            = toInteger . length
    remainingArgs                  = drop (length params) args
    bindVarArgs (Just argName) env = liftIO (bindVars env [(argName, List remainingArgs)])
    bindVarArgs Nothing        env = return env
    evalBody env                   = fmap last (mapM (eval env) body)
apply x _ = throwError (TypeMismatch "func" x)

ifExp :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
ifExp env predicate consequent alternate =
  let g (Bool True)  = eval env consequent
      g (Bool False) = eval env alternate
      g x            = throwError (TypeMismatch "bool" x)
  in eval env predicate >>= g

condExp :: Env -> [LispVal] -> IOThrowsError LispVal
condExp env [List [Atom "else", consequent]]    = eval env consequent
condExp env (List [predicate, consequent] : xs) =
  let g (Bool True)  = eval env consequent
      g (Bool False) = condExp env xs
      g x            = throwError (TypeMismatch "bool" x)
  in eval env predicate >>= g
condExp _ x = throwError (NumArgs 1 x)

caseExp :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
caseExp env _       (List (Atom "else" : thenBody) : _)       = fmap last (mapM (eval env) thenBody)
caseExp env valExpr (List (List datums : thenBody) : clauses) = do
  result     <- eval env valExpr
  let f x = eqv [result, x] >>= unpackBool
  foundMatch <- liftThrows (fmap or (traverse f datums))
  if foundMatch
    then fmap last (mapM (eval env) thenBody)
    else caseExp env valExpr clauses
caseExp _ _       x @ []  = throwError (NumArgs 1 x)
caseExp _ valExpr clauses = throwError (BadSpecialForm "Ill-constructed case expression" (List (Atom "case" : valExpr : clauses)))

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   value @ (String _)                                                     = return value
eval _   value @ (Number _)                                                     = return value
eval _   value @ (Bool _)                                                       = return value
eval env (Atom x)                                                               = getVar env x
eval _   (List [Atom "quote", value])                                           = return value
eval env (List [Atom "if", predicate, consequent, alternate])                   = ifExp env predicate consequent alternate
eval env (List (Atom "cond" : clauses))                                         = condExp env clauses
eval env (List (Atom "case" : key : clauses))                                   = caseExp env key clauses
eval env (List [Atom "set!", Atom var, form])                                   = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form])                                 = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body))               = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body))                            = makeNormalFunc env params body 
eval env (List (Atom "lambda" : DottedList params varargs : body))              = makeVarargs varargs env params body 
eval env (List (Atom "lambda" : varargs @ (Atom _) : body))                     = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename])                                  = load filename >>= fmap last . mapM (eval env)
eval env (List (f : args)) = do
  func    <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)
