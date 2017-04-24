{-# LANGUAGE OverloadedStrings #-}

module Deviser.Parser.Tests
  ( parserTests
  ) where

import Data.Array (listArray)
import Data.Either (isLeft)
import Deviser.Parser (readExpr)
import Deviser.Types (LispVal(..))
import Test.Dwergaz

parseSingleAtom :: Test
parseSingleAtom =
  Expect "Parse a single atom"
         (==)
         (Right (Atom "atom"))
         (readExpr "atom")

parseQuotedSymbol :: Test
parseQuotedSymbol =
  Expect "Parse a quoted symbol"
         (==)
         (Right (List [Atom "quote", Atom "atom"]))
         (readExpr "'atom")

parseListOfSingleAtom :: Test
parseListOfSingleAtom =
  Expect "Parse a list containing a single atom"
         (==)
         (Right (List [Atom "bang"]))
         (readExpr "(bang)")

parseFunctionApplication :: Test
parseFunctionApplication =
  Expect "Parse a function application"
         (==)
         (Right (List [Atom "+", Number 12, Number 13]))
         (readExpr "(+ 12 13)")

parseListOfNumbers :: Test
parseListOfNumbers =
  Expect "Parse a list of numbers"
         (==)
         (Right (List [Atom "list", Number 12, Number 13]))
         (readExpr "(list 12 13)")

parseVectorOfNumbers :: Test
parseVectorOfNumbers =
  Expect "Parse a vector of numbers"
         (==)
         (Right (Vector (listArray (0, 1) [Number 12, Number 13])))
         (readExpr "#(12 13)")

parseBadVector :: Test
parseBadVector =
  Predicate "Floating hash" isLeft (readExpr "# (12 13)")

parseQuotedList :: Test
parseQuotedList =
  Expect "Parse a quoted list (1)"
         (==)
         (Right (List [Atom "quote", List [Number 12, Number 13]]))
         (readExpr "'(12 13)")

parseQuotedList2 :: Test
parseQuotedList2 =
  Expect "Parse a quoted list (2)"
         (==)
         (Right (List [Atom "quote", List [Number 12, Number 13]]))
         (readExpr "' (12 13)")

parseDottedList :: Test
parseDottedList =
  Expect "Parse a dotted list"
         (==)
         (Right (DottedList [Number 12] (Number 13)))
         (readExpr "(12 . 13)")

parseDottedList2 :: Test
parseDottedList2 =
  Expect "Parse a dotted list"
         (==)
         (Right (DottedList [Number 12, Number 14] (Number 13)))
         (readExpr "(12 14 . 13)")

parseNumber :: Test
parseNumber =
  Expect "Parse a number"
         (==)
         (Right (Number 42))
         (readExpr "42")

parseFloat :: Test
parseFloat =
  Expect "Parse a float"
         (==)
         (Right (Float 42.42))
         (readExpr "42.42")

parseString :: Test
parseString =
  Expect "Parse a string"
         (==)
         (Right (String "goliath"))
         (readExpr "\"goliath\"")

parserTests :: [Test]
parserTests =
  [ parseSingleAtom
  , parseQuotedSymbol
  , parseListOfSingleAtom
  , parseFunctionApplication
  , parseListOfNumbers
  , parseVectorOfNumbers
  , parseBadVector
  , parseQuotedList
  -- , parseQuotedList2
  -- , parseDottedList
  -- , parseDottedList2
  , parseNumber
  , parseFloat
  , parseString
  ]
