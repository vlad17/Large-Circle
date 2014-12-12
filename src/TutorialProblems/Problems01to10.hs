-- Problems 1 to 10
-- https://www.haskell.org/haskellwiki/99_questions/1_to_10
-- I'll intentionally solve these with recursion rather than library
-- functions for learning purposes. After this set I'll move on
-- to using the library functions.

module TutorialProblems.Problems01to10 where

-- Problem 1

myLast [] = error "myLast []: empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2

myButLast [] = error "myButLast []: size 0 list"
myButLast [_] = error "myButLast [_]: size 1 list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3

elementAt [] _ = error "elementAt []: empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) n
  | n < 1 = error "elementAt xs n: n < 1"
  | otherwise = elementAt xs $ n - 1

-- Problem 4                
