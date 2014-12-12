-- Problems 1 to 10
-- https://www.haskell.org/haskellwiki/99_questions/1_to_10

module TutorialProblems.Problems01to10 where

-- Problem 1

myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

