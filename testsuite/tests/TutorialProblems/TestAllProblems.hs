{-# LANGUAGE TemplateHaskell #-}
-- Uses QuickCheck to test all the 99 problems.

module Main where 

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import TutorialProblems.AllProblems

main :: IO ()
main = $(defaultMainGenerator)

---- QuickCheck2 test properties

checkError :: a -> Property
checkError x = expectFailure $ seq x True

-- Problem 1
prop_myLast :: [Int] -> Property
prop_myLast [] = checkError $ myLast []
prop_myLast xs = property $ myLast xs == last xs

-- Problem 2
prop_myButLast :: [Int] -> Property
prop_myButLast [] = checkError $ myButLast []
prop_myButLast [x] = checkError $ myButLast [x]
prop_myButLast xs = property $ myButLast xs == (last . init $ xs)

-- Problem 3
prop_elementAt :: [Int] -> Int -> Property
prop_elementAt [] n = checkError $ elementAt [] n
prop_elementAt xs n
  | n < 1 = checkError $ elementAt xs n
  | otherwise = property $ elementAt xs n == xs !! n - 1


