{-# LANGUAGE TemplateHaskell #-}
-- Uses QuickCheck, HUnit to test all the 99 problems.

module Main where

import qualified Data.List as List

import Test.Framework (testGroup, Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assert, Assertion, (@=?))
import Test.QuickCheck

import TutorialProblems.AllProblems

main :: IO ()
main = $(defaultMainGenerator)

---- QuickCheck2 test properties

checkError :: a -> Property
checkError x = expectFailure $ seq x True

checkEqual :: Eq a => (b -> a) -> (b -> a) -> b -> Property
checkEqual f g x = property $ f x == g x
checkEqual2 :: Eq a => (b -> c -> a) -> (b -> c -> a) -> b -> c -> Property
checkEqual2 f g = curry $ checkEqual (uncurry f) (uncurry g)

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
                 [("Nonempty", "abdeghk", ("abcdefghik", 3 :: Int)),
                  ("Empty", "", ("", 4))]

-- Problem 17
prop_split :: [Int] -> Int -> Property
prop_split = checkEqual2 split $ flip splitAt

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

-- Problem 21
case_insertAt :: Assertion
case_insertAt = "aXbcd" @=? insertAt 'X' "abcd" 2

-- Problem 22
prop_range :: Int -> Int -> Property
prop_range x y = abs x < 50 && abs y < 50 ==> checkEqual2 range enumFromTo x y

-- Problem 23
-- Problem 24
-- Problem 25
-- Randomized algorithms, too lazy to test them (that's the Haskell spirit!)

-- Problem 26
prop_combinations :: Int -> Int -> Property
prop_combinations n m = n >= 0 && n <= m && m < 10 ==>
  length combo == binom m n
  && (and . map ((== n) . length)) combo
  && length (List.nub combo) == binom m n
  where combo = combinations n [1..m] :: [[Int]]
        binom _ 0 = 1
        binom x y =
          if x == y then 1 else binom (pred x) (pred y) + binom (pred x) y

-- Problem 27
prop_group :: [Int] -> [Int] -> Property
prop_group ns xs = let (lns, lxs) = (length ns, length xs) in
  lns < lxs && lxs < 10 && all (<= lxs) ns && sum ns <= lxs ==>
  let (osol, mysol) = (online ns xs, group ns xs)
  in List.sort osol == List.sort mysol
  where
    online [] = const [[]]
    online (i:is) = concatMap (uncurry $ (. group is) . map . (:)) . paircomb i

-- Problem 28
test_lsort :: [Test]
test_lsort = testCases lsort
  [("Empty", [], []),
   ("Nonempty", ["o","de","de","mn","abc","fgh","ijkl"],
    ["abc","de","fgh","de","ijkl","mn","o"])]

test_lfsort :: [Test]
test_lfsort = testCases lfsort
  [("Empty", [], []),
   ("Nonempty", ["o","ijkl","abc","fgh","de","de","mn"],
    ["abc", "de", "fgh", "de", "ijkl", "mn", "o"])]

-- Problem 31
test_isPrime :: [Test]
test_isPrime = testCases isPrime
  [("1", False, 1 :: Int), ("2", True, 2), ("3", True, 3), ("4", False, 4),
   ("5", True, 5), ("6", False, 6), ("7", True, 7), ("8", False, 8),
   ("29", True, 29), ("30", False, 30), ("11083", True, 11083)]

-- Problem 32
prop_myGCD :: Int -> Int -> Property
prop_myGCD n m = checkEqual2 myGCD gcd n m

-- Problem 33
case_coprime1 :: Assertion
case_coprime1 = assert $ coprime 35 (64 :: Int)
case_coprime2 :: Assertion
case_coprime2 = assert $ not $ coprime 36 (64 :: Int)

-- Problem 34
case_totient1 :: Assertion
case_totient1 = 4 @=? totient 10
case_totient2 :: Assertion
case_totient2 = 1 @=? totient 1

-- Problem 35
prop_primeFactors :: Int -> Property
prop_primeFactors n = abs n < 10000 ==>
  let primef = primeFactors n in
  property $ n == 0 && primef == [] ||
    product primef == abs n && all isPrime primef

-- Problem 36
case_primeFactorsMult :: Assertion
case_primeFactorsMult = [(3, 2), (5, 1), (7, 1)] @=? primeFactorsMult 315

-- Problem 37
prop_totient2 :: Int -> Property
prop_totient2 n = n > 0 && n < 10000 ==> checkEqual totient totient2 n

-- Problem 38
case_compareTotients :: Assertion
case_compareTotients = assert compareTotients

-- Problem 39
case_primesR :: Assertion
case_primesR = [11, 13, 17, 19 :: Int] @=? primesR 10 20

-- Problem 40
prop_goldbach :: Int -> Property
prop_goldbach n = 2 < n && even n && n < 10000 ==> let (a, b) = goldbach n in
  property $ a + b == n && isPrime a && isPrime b

-- Problem 41
case_goldbachList :: Assertion
case_goldbachList = [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)] @=?
  goldbachList 9 (20 :: Int)
case_goldbachListFast :: Assertion
case_goldbachListFast = [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)] @=?
  goldbachListFast 9 (20 :: Int)
case_goldbachList' :: Assertion
case_goldbachList' = [(73, 919), (61, 1321), (67, 1789), (61, 1867)] @=?
  goldbachList' 4 2000 (50 :: Int)
