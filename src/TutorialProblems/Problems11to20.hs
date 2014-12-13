-- Problems 11 to 20
-- https://www.haskell.org/haskellwiki/99_questions/11_to_20

module TutorialProblems.Problems11to20 where

import qualified TutorialProblems.Problems01to10

data ListItem a = Single a | Multiple Int a
  deriving (Show, Eq)
tupleItem :: ListItem a -> (Int, a)
tupleItem (Single a) = (1, a)
tupleItem (Multiple n a) = (n, a)
itemTuple :: (Int, a) -> ListItem a
itemTuple (1, x) = Single x
itemTuple (n, x) = Multiple n x

-- Problem 11
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map itemTuple . TutorialProblems.Problems01to10.encode

-- Problem 12  
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap $ uncurry replicate . tupleItem

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) =
  let aux tup [] = [itemTuple tup]
      aux t@(n, y) (x:xs)
        | y == x = aux (succ n, y) xs
        | otherwise = itemTuple t : aux (1, x) xs
  in aux (1, x) xs

-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\ x y -> x:x:y) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli = flip $ concatMap . replicate

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n =
  let (top, bottom) = splitAt (pred n) xs
  in top ++ dropEvery (drop 1 bottom) n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs lo hi = take (hi - lo + 1) . drop (lo - 1) $ xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = take len $ drop shift $ cycle xs
              where shift = len + n `mod` len
                    len = length xs

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! pred n, take (pred n) xs ++ drop n xs)
