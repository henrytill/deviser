{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad.Except
import Data.Array
import Data.Complex
import Data.IORef
import Data.Maybe (isJust)
import Data.Ratio
import Numeric
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

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

data LispError =
    NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String


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

instance Show LispVal where
    show = showVal

showError :: LispError -> String
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " value, found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (Default message)             = "Default error: " ++ message

instance Show LispError where
    show = showError


-- Error Handling

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error ("extractValue: this should never happen! " ++ show err)


-- Environment Handling

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrowsError :: IOThrowsError String -> IO String
runIOThrowsError action = fmap extractValue (runExceptT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
    liftIO (readIORef envRef) >>=
    maybe err (liftIO . readIORef) . lookup var
  where
    err = throwError (UnboundVar "Getting an unbound variable" var)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
    liftIO (readIORef envRef) >>=
    maybe err (liftIO . flip writeIORef value) . lookup var >>
    return value
  where
    err = throwError (UnboundVar "Setting an unbound variable" var)

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO (isBound envRef var)
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
        valueRef <- newIORef value
        env      <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    addBindings :: (String, LispVal) -> IO (String, IORef LispVal)
    addBindings (var, value) = newIORef value >>= \ref -> return (var, ref)
    extendEnv :: [(String, LispVal)] -> [(String, IORef LispVal)] -> IO [(String, IORef LispVal)]
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


-- List primitives

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


-- Unpackers

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError (TypeMismatch "number" x)

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
-- unpackStr (Number n) = return (show n)
-- unpackStr (Bool b)   = return (show b)
unpackStr x          = throwError (TypeMismatch "string" x)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool x        = throwError (TypeMismatch "bool" x)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
    ((==) <$> unpacker a <*> unpacker b) `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1 @ (List _), l2 @ (List _)] = eqList equal [l1, l2]
equal [a, b]                         =
    let unpackers       = [ AnyUnpacker unpackNum
                          , AnyUnpacker unpackStr
                          , AnyUnpacker unpackBool
                          ]                                               :: [Unpacker]
        primitiveEquals = fmap or (traverse (unpackEquals a b) unpackers) :: ThrowsError Bool
        eqvEquals       = eqv [a, b] >>= unpackBool                       :: ThrowsError Bool
    in Bool <$> ((||) <$> primitiveEquals <*> eqvEquals)
equal x                              = throwError (NumArgs 2 x)


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

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    , ("car",            car)
    , ("cdr",            cdr)
    , ("eqv?",           eqv)
    , ("eq?",            eqv)
    , ("equal?",         equal)
    ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe err (\f -> f args) (lookup func primitives)
  where
    err = throwError (NotFunction "Unrecognized primitive function" func)

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
condExp _ x                                   = throwError (NumArgs 1 x)

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
eval _   value @ (String _)                                   = return value
eval _   value @ (Number _)                                   = return value
eval _   value @ (Bool _)                                     = return value
eval env (Atom x)                                             = getVar env x
eval _   (List [Atom "quote", value])                         = return value
eval env (List [Atom "if", predicate, consequent, alternate]) = ifExp env predicate consequent alternate
eval env (List (Atom "cond" : clauses))                       = condExp env clauses
eval env (List (Atom "case" : key : clauses))                 = caseExp env key clauses
eval env (List [Atom "set!", Atom var, form])                 = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form])               = eval env form >>= defineVar env var
eval env (List (Atom f : args))                               = mapM (eval env) args >>= liftThrows . apply f
eval _   badForm                                              = throwError (BadSpecialForm "Unrecognized special form" badForm)


-- Helpers

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


-- Atom

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return (Atom atom)


-- Lists

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr spaces)

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return (List [Atom "quote", x])

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
    _ <- char '`'
    x <- parseExpr
    return (List [Atom "quasiquote", x])

parseUnquoted :: Parser LispVal
parseUnquoted = do
    _ <- char ','
    x <- parseExpr
    return (List [Atom "unquote", x])

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return (DottedList h t)

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    _ <- char '('
    x <- try parseList <|> parseDottedList
    _ <- char ')'
    return x


-- Vector

parseVector :: Parser LispVal
parseVector = try $ do
    _  <- string "#("
    xs <- sepBy parseExpr spaces
    _  <- char ')'
    return (Vector (listArray (0, length xs - 1) xs))


-- Number

parseDigit1 :: Parser LispVal
parseDigit1 = (Number . read) <$> many1 digit

parseDigit2 :: Parser LispVal
parseDigit2 = do
    _ <- try (string "#d")
    x <- many1 digit
    return (Number (read x))

hexDigitToNum :: (Num a, Eq a) => String -> a
hexDigitToNum = fst . head . readHex

parseHex :: Parser LispVal
parseHex = do
    _ <- try (string "#x")
    x <- many1 hexDigit
    return (Number (hexDigitToNum x))

octDigitToNum :: (Num a, Eq a) => String -> a
octDigitToNum = fst . head . readOct

parseOct :: Parser LispVal
parseOct = do
    _ <- try (string "#o")
    x <- many1 octDigit
    return (Number (octDigitToNum x))

binDigitsToNum' :: Num t => t -> String -> t
binDigitsToNum' digint ""     = digint
binDigitsToNum' digint (x:xs) = binDigitsToNum' old xs
  where
    old = 2 * digint + (if x == '0' then 0 else 1)

binDigitsToNum :: String -> Integer
binDigitsToNum = binDigitsToNum' 0

parseBin :: Parser LispVal
parseBin = do
    _ <- try (string "#b")
    x <- many1 (oneOf "10")
    return (Number (binDigitsToNum x))


-- Float

floatDigitsToDouble ::  String -> String -> Double
floatDigitsToDouble i d = fst (head (readFloat (i ++ "." ++ d)))

parseFloat :: Parser LispVal
parseFloat = do
    i <- many1 digit
    _ <- char '.'
    d <- many1 digit
    return (Float (floatDigitsToDouble i d))


-- Ratio

ratioDigitsToRational :: String -> String -> Rational
ratioDigitsToRational n d = read n % read d

parseRatio :: Parser LispVal
parseRatio = do
    n <- many1 digit
    _ <- char '/'
    d <- many1 digit
    return (Ratio (ratioDigitsToRational n d))


-- Complex

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n
toDouble x          = error ("toDouble not implemented for " ++ show x)

lispValsToComplex :: LispVal -> LispVal -> Complex Double
lispValsToComplex x y = toDouble x :+ toDouble y

parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> parseDigit1
    _ <- char '+'
    y <- try parseFloat <|> parseDigit1
    _ <- char 'i'
    return (Complex (lispValsToComplex x y))


-- String

escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
               '\\' -> x
               '"'  -> x
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'
               _    -> error "impossible"

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (escapedChars <|> noneOf "\"\\")
    _ <- char '"'
    return (String x)


-- Character

parseCharacter :: Parser LispVal
parseCharacter = do
    _     <- try (string "#\\")
    value <- try (string "newline" <|> string "space") <|>
             do
                 x <- anyChar
                 _ <- notFollowedBy alphaNum
                 return [x]
    return $ case value of
               "space"   -> Character ' '
               "newline" -> Character '\n'
               _         -> Character (head value)


-- Boolean

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


-- Composed parsers

parseNumber :: Parser LispVal
parseNumber =
    try parseComplex
    <|> try parseRatio
    <|> try parseFloat
    <|> parseDigit1
    <|> parseDigit2
    <|> parseHex
    <|> parseOct
    <|> parseBin

parseExpr :: Parser LispVal
parseExpr =
    parseAtom
    <|> parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuoted
    <|> parseListOrDottedList
    <|> parseQuasiquoted
    <|> parseUnquoted
    <|> parseVector

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError (Parser err)
    Right val -> return val


-- REPL

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

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runREPL :: IO ()
runREPL = nullEnv >>= until_ (== "quit") (readPrompt ">>> ") . evalAndPrint


-- main

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> runREPL
      1 -> runOne (head args)
      _ -> putStrLn "Program only takes 0 or 1 argument"
