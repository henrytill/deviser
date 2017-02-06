module Deviser.Parser where

import Control.Monad.Except (throwError)
import Data.Array (listArray)
import Data.Complex
import Data.Ratio ((%))
import Numeric (readFloat, readHex, readOct)
import Text.Parsec hiding (spaces)
import Deviser.Types

type Parser = Parsec String ()

-- Helpers

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

newlines :: Parser ()
newlines = skipMany1 newline

maybeSpaces :: Parser ()
maybeSpaces = skipMany space

maybeNewlines :: Parser ()
maybeNewlines = skipMany newline


-- Atom

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return (Atom atom)


-- Lists

parseList :: Parser LispVal
parseList = fmap List (maybeSpaces *> many (parseExpr <* maybeSpaces))

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
    _ <- maybeSpaces
    x <- parseList <|> parseDottedList
    _ <- maybeSpaces
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
             do x <- anyChar
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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse (maybeSpaces >> parser) "lisp" input of
    Left err  -> throwError (Syntax err)
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
