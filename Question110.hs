module Question110 where

-- Question 1 Find the last element of a list

lastElement :: [a] -> a
lastElement list = case list of
  []      -> error "No end for empty lists!!"
  [x]     -> x
  ( _:xs )  -> lastElement xs

-- Question 2 Find the last but one element of a list

myButLast :: [a] -> a
myButLast list = case list of
    []          -> error "No elements within a list"
    [x,y]       -> x
    ( _:xs )    -> myButLast xs

{- Question 3 Find the kth element of a list given the first element in the list is number 1

--{elementAt :: [a] -> Integer -> a
elementAt list num
    | num == 0    = error "Cannot have the oth number of a list"
    | list == []  = error "Cannot have an empty list"
    | ( x:_ ) 1   = x
    | ( _:xs ) k
           | k < 1 = error "index out of bounds"
           | other = elementAt' xs (k-1)"
-}

-- Question 4 Find the number of elements of a list

myLength :: [a] -> Integer
myLength list = case list of
    []  -> 0
    [x] -> 1
    ( _:xs ) -> 1 + myLength xs

-- Question 5 Reverse a list

myReverse :: [a] -> [a]
myReverse  a = case a of
    [] -> []
    (x:xs) -> reverse xs ++ [x]

-- Question 6 Find out whether a list is a palindrome. A palindrome can be read forwards or backwards.
--https://stackoverflow.com/questions/16154592/haskell-no-instance-for-eq-a-arising-from-a-use-of
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a
    | a == [] = True
    | myReverse a == a = True
    | otherwise        = False
    


-- Question 8 Eliminates consecutive duplicates of list element
compress :: Eq a => [a] -> [a]
compress a = case a of
    [] -> []
    [x] -> [x]
    (x:y:xs)
        | (x == y)  -> compress (y:xs)
        | otherwise -> [x] ++ compress (y:xs)

 