{-# LANGUAGE TemplateHaskell #-}
-- Uses QuickCheck, HUnit to test all the 99 problems.

module Main where 

import qualified Data.List as List

import Test.Framework (testGroup, Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, (@=?))
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
testCases f = map $ \(str, a, b) -> testCase str $ a @=? f b 

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
test_flatten :: [Test]
test_flatten = testCases flatten
               [("Elem 5", [5] :: [Int], Elem 5),
                ("List []", [], List []),
                ("1-5", [1, 2, 3, 4, 5],
                 List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]),
                ("Nested", [], List [List [List [], List [List []]]])]

-- Problem 8
test_compress :: [Test]
test_compress = testCases compress
                [("Empty", [], [] :: [Int]),
                 ("Same", [1], [1, 1, 1, 1, 1]),
                 ("One Different", [1, 2, 1], [1, 1, 2, 1, 1, 1]),
                 ("Different", [1, 2, 3, 1, 4, 5],
                  [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5])]
                
-- Problem 9
prop_pack :: [[Int]] -> Property
prop_pack = checkEqual pack List.group

-- Problem 10
test_encode :: [Test]
test_encode = testCases encode
              [("Empty", [], [] :: [Int]),
               ("Same", [(5, 1)], [1, 1, 1, 1, 1]),
               ("One Different", [(2, 1), (1, 2), (2, 1)], [1, 1, 2, 1, 1]),
               ("Different", [(4, 1), (1, 2), (2, 3), (2, 1), (1, 4), (4, 5)],
                [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5])]

-- Problem 11
test_encodeModified :: [Test]
test_encodeModified = testCases encodeModified
  [("Nonempty", [Multiple 4 'a', Single 'b', Multiple 2 'c', 
                 Multiple 2 'a', Single 'd', Multiple 4 'e'], "aaaabccaadeeee"),
   ("Empty", [], ""),
   ("Same 5", [Multiple 5 'a'], "aaaaa"),
   ("Same 1", [Single 'a'], "a")]

-- Problem 12
test_decodeModified :: [Test]
test_decodeModified = testCases decodeModified
  [("Nonempty", "aaaabccaadeeee", [Multiple 4 'a', Single 'b', Multiple 2 'c', 
                                   Multiple 2 'a', Single 'd', Multiple 4 'e']),
   ("Empty", "", []),
   ("Same 5", "aaaaa", [Multiple 5 'a']),
   ("Same 1", "a", [Single 'a'])]

-- Problem 13
prop_encodeDirect :: [Int] -> Property
prop_encodeDirect = checkEqual encodeDirect encodeModified

-- Problem 14
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = (\ (x, y) -> x : chunk n y) $ splitAt n xs

prop_dupli :: [Int] -> Property
prop_dupli xs = let dup = chunk 2 $ dupli xs
                in property $ map head dup == xs && all ((== 2) . length) dup

-- Problem 15
prop_repli2 :: [Int] -> Property
prop_repli2 = checkEqual dupli $ flip repli 2
prop_repli4 :: [Int] -> Property
prop_repli4 = checkEqual (dupli . dupli) $ flip repli 4

-- Problem 16
test_dropEvery :: [Test]
test_dropEvery = testCases (uncurry dropEvery)
                 [("Nonempty", "abdeghk", ("abcdefghik", 3)),
                  ("Empty", "", ("", 4))]

-- Problem 17
prop_split :: ([Int], Int) -> Property
prop_split = checkEqual (uncurry split) (uncurry $ flip splitAt)

-- Problem 18
case_slice :: Assertion
case_slice = "cdefg" @=? slice ['a','b','c','d','e','f','g','h','i','k'] 3 7

-- Problem 19
case_rotate1 :: Assertion
case_rotate1 = "defghabc" @=? rotate ['a','b','c','d','e','f','g','h'] 3
case_rotate2 :: Assertion
case_rotate2 = "ghabcdef" @=? rotate ['a','b','c','d','e','f','g','h'] (-2)

-- Problem 20
case_removeAt :: Assertion
case_removeAt = ('b',"acd") @=? removeAt 2 "abcd"
