{-# LANGUAGE TemplateHaskell #-}
-- Uses QuickCheck to test all the 99 problems.

module Main where 

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import TutorialProblems.AllProblems

main :: IO ()
main = $(defaultMainGenerator)

---- QuickCheck2 test properties

-- myLast should be equivalent to last
prop_myLast :: [Int] -> Bool
prop_myLast [] = True -- expectFailure . myLast $ []
prop_myLast xs = myLast xs == last xs
