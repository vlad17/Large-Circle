-- Problems 21 to 28
-- https://www.haskell.org/haskellwiki/99_questions/21_to_28

module TutorialProblems.Problems21to28 where

import Control.Applicative ((<*>))
import Control.Monad (return, (>>=), mapM)
import Data.Function (on)
import qualified Data.Ord as Ord
import qualified Data.List as List
import System.Random (randomRIO)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n =
  let (front, back) = List.splitAt (pred n) xs in front ++ x : back

-- Problem 22
range :: Int -> Int -> [Int]
range lo hi  
  | lo > hi = []
  | otherwise = lo : range (succ lo) hi

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return [] -- note this base will always be reached
rndSelect (x:xs) n = do -- clever algorithm below from solutions
  r <- randomRIO (0, length xs)
  if r < n -- online probability of selection is spots left / num remaining
    then rndSelect xs (pred n) >>= return . (x :)
    else rndSelect xs n

-- Problem 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- Problem 25
rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu [x] = return [x]
rndPermu xs = do
  r <- randomRIO (0, pred $ length xs)
  comb <- let (ys, x:zs) = splitAt r xs
          in mapM rndPermu [[x], ys, zs]
  return $ foldl (++) [] comb  

-- Problem 26
-- Helper from https://www.haskell.org/haskellwiki/99_questions/Solutions/27
paircomb 0 xs     = [([],xs)]
paircomb n []     = []
paircomb n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- paircomb (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- paircomb  n    xs ]
  
combinations :: Int -> [a] -> [[a]]
combinations 0 = const [[]]
combinations n = map fst . paircomb n 

-- Problem 27
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group ns xs = concatMap (partitions ns) $ combinations (sum ns) xs
  where partitions [] _ = [[]]
        partitions (n:ns) xs =
          concatMap (\ (cut, rest) -> map (cut :) $ partitions ns rest) $
          paircomb n xs
            
-- Problem 28
lsort :: [[a]] -> [[a]]
lsort = map fst . List.sortBy (Ord.comparing snd) . map ((,) <*> length)

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . List.groupBy ((==) `on` length) . lsort
