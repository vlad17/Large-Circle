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

-- myLast should be equivalent to last
prop_myLast :: [Int] -> Property
prop_myLast [] = expectFailure $ seq (myLast []) True
prop_myLast xs = property $ myLast xs == last xs
