-- Problems 1 to 10
-- https://www.haskell.org/haskellwiki/99_questions/1_to_10

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
  | otherwise = elementAt xs $ pred n

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = succ $ myLength xs

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
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x

-- Problem 8
compress :: Eq a => [a] -> [a]
compress (x:xs@(y:_))
  | x == y = compress xs
  | otherwise = x : compress xs
compress xs = xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (group, rest) = span (== x) xs in (x:group) : pack rest

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\ x -> (length x, head x)) . pack

