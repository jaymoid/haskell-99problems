module Q11to20ListsContinued where

-- https://wiki.haskell.org/99_questions/11_to_20

import Q01to10Lists

data ListItem a = Single a
                | Multiple Int a
                  deriving (Show, Eq)

-- 11
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map (uncurry toListItem) . encode
  where toListItem n e = case n of 1 -> Single e
                                   _ -> Multiple n e


-- 12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap fromListItem
  where fromListItem (Single i)     = [i]
        fromListItem (Multiple n i) = replicate n i


-- 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect []       = []
encodeDirect xs@(x:_) = firstGroup ++ encodeDirect restOfList
  where
    firstGroup = (: []) $ toListItem $ takeWhile (== x) xs
    restOfList = dropWhile (== x) xs

toListItem :: [a] -> ListItem a
toListItem xs@(x:_) = case length xs of
        1         -> Single x
        otherwise -> Multiple (length xs) x


-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)


-- 15
repli :: (Integral b) => [a] -> b -> [a]
repli [] _ = []
repli (x:xs) n = replicateElement n x ++ repli xs n
  where
    replicateElement 0 _ = []
    replicateElement n x = x : replicateElement (n-1) x


-- 16
dropEvery :: (Integral b) => [a] -> b -> [a]
dropEvery xs skipEvery = go xs 1
  where
    go []     _ = []
    go (x:xs) n
      | n `mod` skipEvery == 0 = go xs (n+1)
      | otherwise              = x : go xs (n+1)


-- 17
split :: (Integral b) => [a] -> b -> ([a], [a])
split xs n = (firstBit, secondBit)
 where firstBit  = take nInt xs
       secondBit = drop nInt xs
       nInt      = fromIntegral n


-- 18
slice :: (Integral b) => [a] -> b -> b -> [a]
slice xs start end = go xs 1
  where
    go [] _ = []
    go (x:xs) n
      | n >= start && n <= end = x : go xs (n + 1)
      | otherwise = go xs (n + 1)


-- 19
rotate :: (Integral b) => [a] -> b -> [a]
rotate xs n = drop offset xs ++ take offset xs
  where
    offset = calcOffset $ fromIntegral n
    calcOffset x = if x<0 then length xs - abs x else x


-- 20
removeAt :: (Integral a) => a -> [b] -> (b, [b])
removeAt n xs = (xs!!(fromIntegral n - 1), listCreate 1 xs)
  where listCreate _     []     = []
        listCreate count (x:xs) = if count == n then   listCreate (count+1) xs
                                                else x:listCreate (count+1) xs
