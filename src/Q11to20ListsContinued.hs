module Q11to20ListsContinued where

-- https://wiki.haskell.org/99_questions/11_to_20

data ListItem a = Single a
                | Multiple Int a
                  deriving (Show, Eq)

-- 11
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = undefined

-- 12
decodeModified :: [ListItem a] -> [a]
decodeModified = undefined

-- 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = undefined

-- 14
dupli :: [a] -> [a]
dupli = undefined

-- 15
repli :: (Integral b) => [a] -> b -> [a]
repli = undefined

-- 16
dropEvery :: (Integral b) => [a] -> b -> [a]
dropEvery = undefined

-- 17
split :: (Integral b) => [a] -> b -> ([a], [a])
split = undefined

-- 18
slice :: (Integral b) => [a] -> b -> b -> [a]
slice = undefined

-- 19
rotate :: (Integral b) => [a] -> b -> [a]
rotate = undefined

-- 20
removeAt :: (Integral a) => a -> [b] -> (b, [b])
removeAt = undefined
