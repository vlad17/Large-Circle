{-# LANGUAGE TemplateHaskell #-}
-- Uses QuickCheck, HUnit to test all the 99 problems.

module Main where 

import Test.Framework (testGroup, Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import Test.QuickCheck

import TutorialProblems.AllProblems

main :: IO ()
main = $(defaultMainGenerator)

---- QuickCheck2 test properties

checkError :: a -> Property
checkError x = expectFailure $ seq x True

checkEqual :: Eq a => (b -> a) -> (b -> a) -> b -> Property
checkEqual f g x = property $ f x == g x

testCases :: (Show a, Eq a) => (b -> a) -> [(String, a, b)] -> [Test]
testCases f = let mktest (str, a, b) = testCase str $ a @=? f b in map mktest

-- Problem 1
prop_myLast :: [Int] -> Property
prop_myLast [] = checkError $ myLast []
prop_myLast xs = checkEqual myLast last xs

-- Problem 2
prop_myButLast :: [Int] -> Property
prop_myButLast [] = checkError $ myButLast []
prop_myButLast [x] = checkError $ myButLast [x]
prop_myButLast xs = checkEqual myButLast (last . init) $ xs

-- Problem 3
prop_elementAt :: [Int] -> Int -> Property
prop_elementAt [] n = checkError $ elementAt [] n
prop_elementAt xs n
  | n < 1 = checkError $ elementAt xs n
  | otherwise = property $ elementAt xs n == xs !! n - 1

-- Problem 4
prop_myLength :: [Int] -> Property
prop_myLength = checkEqual myLength length

-- Problem 5
prop_myReverse :: [Int] -> Property
prop_myReverse = checkEqual myReverse reverse

-- Problem 6
prop_isPalindromeYes :: [Int] -> Property
prop_isPalindromeYes xs = property $ isPalindrome $ xs ++ reverse xs
prop_isPalindromeNo :: [Int] -> Property
prop_isPalindromeNo xs =
  not (null xs) ==> property $ not . isPalindrome $ [off, off] ++ xs
  where off = last xs + 1

-- Problem 7
test_myFlatten :: [Test]
test_myFlatten = testCases myFlatten
                 [("Elem 5", [5], Elem 5),
                  ("List []", [], List []),
                  ("1-5", [1, 2, 3, 4, 5],
                   List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]),
                  ("Nested", [], List [List [List [], List [List []]]])]

--Problem 8
test_compress :: [Test]
test_compress = testCases compress
                [("Empty", [], []),
                 ("Same", [1], [1, 1, 1, 1, 1]),
                 ("One Different", [1, 2, 1], [1, 1, 2, 1, 1, 1]),
                 ("Different", [1, 2, 3, 1, 4, 5],
                  [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5])]
                  
                
