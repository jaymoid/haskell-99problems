{-# LANGUAGE ParallelListComp #-}
module Q01to10Lists where

-- https://wiki.haskell.org/99_questions/1_to_10

--1
myLast :: [a] -> a
myLast (x:xs) = case xs of
  [] -> x
  _ -> myLast1 xs

myLast1 :: [a] -> a
myLast1 = foldr1 (const id)

myLast2 :: [a] -> a
myLast2 = foldl (flip const) undefined


--2
myButLast :: [a] -> a
myButLast (x1:x2:xs) = case xs of
  [] -> x1
  _  -> myButLast xs

myButLast1 :: [a] -> a
myButLast1 xs = case parse xs of
    Nothing -> error "Not enough values in list"
    Just x  -> x
  where parse = fst . foldl (\b a -> (snd b, Just a)) (Nothing, Nothing)

myButLast2 :: [a] -> a
myButLast2 = last . init


--3
elementAt :: (Integral b) => [a] -> b -> a
elementAt xs index = go 1 xs
  where go count (x:xs)
         | count < index = go (count + 1) xs
         | otherwise = x

elementAt1 :: (Integral b) => [a] -> b -> a
elementAt1 xs n = snd . last $ [(i, x) | x <-xs | i <- [1..n]]

-- or same as above with zip
elementAt2 :: (Integral b) => [a] -> b -> a
elementAt2 xs n = snd . last $ zip [1..n] xs


--4
myLength :: (Integral b) => [a] -> b
myLength xs = go 0 xs
  where go n [] = n
        go n (x:xs) = go (n+1) xs

myLength1 :: (Integral b) => [a] -> b
myLength1 = foldr (\_ count ->  count + 1) 0


--5
myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) =  myReverse xs ++ x : []


--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == myReverse a


-- 7
--In Haskell: We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a =
  Elem a |
  List [NestedList a]
  deriving (Show, Eq)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten1 :: NestedList a -> [a]
flatten1 (Elem x) = [x]
flatten1 (List x) = concatMap flatten x

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = x >>= flatten


--8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) =
  let grouped = if x /= head xs then [x] else []
  in grouped ++ compress xs

compress1 :: Eq a => [a] -> [a]
compress1 = foldr (\e xs -> if (xs == [] || e /= head xs) then e:xs else xs) []


--9
pack :: Eq a => [a] -> [[a]]
pack xs = go [] xs
  where
    go [] [] = []
    go [] (x:xs) = go (x:[]) xs
    go ys [] = [ys]
    go ys (x:xs) = if x `elem` ys then go (x:ys) xs else ys : (go [x] xs)

pack1 :: Eq a => [a] -> [[a]]
pack1 [] = []
pack1 (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)


--10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

encode1 :: Eq a => [a] -> [(Int, a)]
encode1 = foldr convert [] . pack
  where convert a b = (length a, head a) : b
