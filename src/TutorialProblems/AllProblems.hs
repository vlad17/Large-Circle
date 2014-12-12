-- File contains the answers to all the 99 haskell problems

module TutorialProblems.AllProblems where 

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs
