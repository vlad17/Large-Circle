-- Problems 1 to 10
-- https://www.haskell.org/haskellwiki/99_questions/1_to_10
-- I'll intentionally solve these with recursion rather than library
-- functions for learning purposes. After this set I'll move on
-- to using the library functions.

module TutorialProblems.Problems01to10 where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "myLast []: empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "myButLast []: size 0 list"
myButLast [_] = error "myButLast [_]: size 1 list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "elementAt []: empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) n
  | n < 1 = error "elementAt xs n: n < 1"
  | otherwise = elementAt xs $ n - 1

-- Problem 4
myLength :: [a] -> Int
myLength = let aux [] acc = acc
               aux (_:xs) acc = aux xs $ acc + 1
           in flip aux 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = let aux [] acc = acc
                aux (x:xs) acc = aux xs $ x:acc
            in flip aux []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten nl =
  let aux (Elem a) acc = a:acc
      aux (List []) acc = acc
      aux (List (x:xs)) acc = aux (List xs) $ aux x acc
  in reverse $ aux nl []
