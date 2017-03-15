{-# LANGUAGE GADTs #-}

module Dwergaz where

data Test where
  Predicate :: (Eq a, Show a) => String -> (a -> Bool) -> a -> Test
  Expect    :: (Eq a, Show a) => String -> (a -> a -> Bool) -> a -> a -> Test

data Result where
  Passed :: String -> Result
  Failed :: Show a => String -> a -> a -> Result

instance Show Result where
  show (Failed n e a) =
    "FAILED:   "     ++ n      ++
    "\nEXPECTED:   " ++ show e ++
    "\nACTUAL:     " ++ show a
  show (Passed n) =
    "PASSED:   "     ++ n

isPassed :: Result -> Bool
isPassed (Passed _) = True
isPassed _          = False

runTest :: Test -> Result
runTest (Predicate n p v)
  | p v                   = Passed n
  | otherwise             = Failed n True False
runTest (Expect n f e a)
  | f e a                 = Passed n
  | otherwise             = Failed n e a
