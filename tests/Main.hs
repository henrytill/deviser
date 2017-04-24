module Main (main) where

import Control.Monad
import Deviser.Parser.Tests
import Test.Dwergaz (isPassed, runTest, Result)
import System.Exit

results :: [Result]
results = fmap runTest parserTests

main :: IO ()
main = do
  _ <- mapM_ print results
  unless (all isPassed results) exitFailure
