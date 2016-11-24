module Main where

import Control.Monad.Except
import Data.Array
import Data.Complex
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
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

data LispError = NumArgs Integer [LispVal]
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
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
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


-- Evaluator

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError (TypeMismatch "number" x)

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _  param @ [_] = throwError (NumArgs 2 param)
numericBinOp op params      = fmap (Number . foldl1 op) (mapM unpackNum params)

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return (f v)
unaryOp _ x   = throwError (NumArgs 1 x)

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
    ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe err (\f -> f args) (lookup func primitives)
  where
    err = throwError (NotFunction "Unrecognized primitive function" func)

eval :: LispVal -> ThrowsError LispVal
eval val @ (String _)           = return val
eval val @ (Number _)           = return val
eval val @ (Bool _)             = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm                    = throwError (BadSpecialForm "Unrecognized special form" badForm)


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

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show (readExpr (head args) >>= eval)
    putStrLn (extractValue (trapError evaled))
