module Q1to10Lists where

--1
myLast :: [a] -> a
myLast = undefined

--2
myButLast :: [a] -> a
myButLast = undefined

--3
elementAt :: (Integral b) => [a] -> b -> a
elementAt = undefined

--4
myLength :: (Integral b) => [a] -> b
myLength = undefined

--5
myReverse :: [a] -> [a]
myReverse = undefined

--6
isPalindrome :: [a] -> Bool
isPalindrome = undefined

-- 7
--In Haskell: We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)

flatten :: NestedList a -> [a]
flatten = undefined

--8
compress :: [a] -> [a]
compress = undefined

--9
pack :: [a] -> [[a]]
pack = undefined

--10
encode :: (Integral a) => [b] -> [(a, b)]
encode = undefined
