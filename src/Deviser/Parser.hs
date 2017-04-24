{-# LANGUAGE OverloadedStrings #-}

module Deviser.Parser
  ( readExpr
  , readExprFile
  ) where

import Data.Array (listArray)
import Data.Complex
import Data.Functor.Identity (Identity)
import Data.Ratio ((%))
import qualified Data.Text as T
import Numeric (readFloat, readHex, readOct)
import Text.Parsec hiding (spaces)
import qualified Text.Parsec.Language as Language
import Text.Parsec.Text
import qualified Text.Parsec.Token as Token
import Deviser.Types

-- Lexer

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

schemeDef :: Token.GenLanguageDef T.Text () Identity
schemeDef = Language.emptyDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ";"
  , Token.opStart         = Token.opLetter schemeDef
  , Token.opLetter        = symbol
  , Token.identStart      = letter <|> symbol
  , Token.identLetter     = letter <|> symbol <|> digit
  , Token.reservedOpNames = ["'", "\"", ".", "+", "-"]
  }

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser schemeDef

reservedOp :: T.Text -> Parser ()
reservedOp op = Token.reservedOp lexer (T.unpack op)

identifier :: Parser String
identifier = Token.identifier lexer

spaces :: Parser ()
spaces = Token.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer


-- Atom

parseAtom :: Parser LispVal
parseAtom = identifier >>= \i -> return (Atom (T.pack i))


-- Lists

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

parseList :: Parser LispVal
parseList = List <$> (spaces *> many (parseExpr <* spaces))

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return (DottedList h t)

parseListOrDottedList :: Parser LispVal
parseListOrDottedList =
  parens (spaces *> (parseList <|> parseDottedList <* spaces))


-- Vector

parseVector :: Parser LispVal
parseVector = try $ do
  _  <- string "#("
  xs <- spaces *> many (parseExpr <* spaces)
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

binDigitsToNum :: String -> Integer
binDigitsToNum = binDigitsToNum' 0
  where
    binDigitsToNum' :: Num t => t -> String -> t
    binDigitsToNum' digint xs =
      case xs of
        ""     -> digint
        (y:ys) -> binDigitsToNum' (2 * digint + (if y == '0' then 0 else 1)) ys

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

parseString :: Parser LispVal
parseString = stringLiteral >>= \x -> return (String (T.pack x))


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
             _         -> Character (T.head (T.pack value))


-- Boolean

parseBool :: Parser LispVal
parseBool =
  char '#' *> ((char 't' *> return (Bool True)) <|> (char 'f' *> return (Bool False)))


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

withSpaces :: Parser a -> Parser a
withSpaces p = spaces *> p <* spaces

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (withSpaces parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (withSpaces parseList) "<file>"
