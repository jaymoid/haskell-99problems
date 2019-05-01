module Q21to28ListsAgain where

import System.Random (StdGen(), mkStdGen, randomR, randomIO)
import Control.Monad.Trans.State (State(), state, evalState)
import Safe (atMay)
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)

-- 21
insertAt :: (Integral b) => a -> [a] -> b -> [a]
insertAt e xs index = foldr f [] (zip xs [1..])
  where f x acc = let (thing, pos) = x
                  in if pos == fromIntegral index then e:thing:acc else thing:acc

insertAt' e xs index = let pos = fromIntegral index - 1
                       in take pos xs ++ e : drop pos xs

insertAt'' e xs index = go 0 xs
  where
    go n [] = []
    go n (x:xs) = if n == index - 1 then e : x : go (n + 1) xs else x : go (n + 1) xs



-- 22
range :: (Integral a) => a -> a -> [a]
range from to = if from >= to then [to] else from : range (succ from) to

range' :: (Integral a) => a -> a -> [a]
range' = enumFromTo

range'' from to = [from..to]



-- 23
rndSelect :: (Integral b) => [a] -> b -> [a]
rndSelect xs n = catMaybes $ evalState (nRandomElements xs n) (mkStdGen seed)
  where seed = 100

nRandomElements :: (Integral b) => [a] -> b -> State StdGen [Maybe a]
nRandomElements xs n = replicateM (fromIntegral n) (randomElement xs)
-- >  evalState (nRandomElements [1..10000] 5)  (mkStdGen 3)
-- [Just 9364,Just 4794,Just 7233,Just 7026,Just 3812]

randomElement :: [a] -> State StdGen (Maybe a)
randomElement xs = (\n -> atMay xs n) <$> state (randomR (0, length xs - 1))

-- alternate of above ^
-- randomElement xs = state $ do
--   (n, s) <- randomR (0, length xs - 1)
--   return (xs `atMay` n, s)

-- > evalState (randomElement [1..100]) (mkStdGen 1)
-- Just 36

-- ^ This version relies on a seed being specified, lets see if we can use randomIO
rndSelectIO ::  (Integral b) => [a] -> b -> IO [a]
rndSelectIO xs n = catMaybes . evalState (nRandomElements xs n) <$> randomStdGen
  where
    randomStdGen :: IO StdGen
    randomStdGen = mkStdGen <$> randomIO



-- 24
diffSelect :: (Integral a) => a -> a -> [a]
diffSelect noToSelect maxNo = rndSelect [1..maxNo] noToSelect



-- 25
rndPermu :: [a] -> [a]
rndPermu xs = catMaybes $ evalState (replicateM (length xs) takeRandomElementFromList) (mkStdGen seed, xs)
  where seed = 100

takeRandomElementFromList :: State (StdGen, [a]) (Maybe a)
takeRandomElementFromList = state $ do
  gen <- fst
  xs  <- snd
  let (randomNum, nextGen) = randomR (0, length xs - 1) gen
      randomX = xs `atMay` randomNum
      xsWithoutX = xs `deleteAt` randomNum
  return (randomX, (nextGen, xsWithoutX))

deleteAt :: [a] -> Int -> [a]
deleteAt xs index = left ++ right
  where (left, (_aAtIndex:right)) = splitAt index xs


-- Some playing around with state monad, getting the do syntax right for the above, still bit unsure about do!
test :: State StdGen Int
test = state $ do
  -- stdGen <- fst
  -- num <- snd
  (randomNum, nextStdGen) <- randomR(0, 10)
  return (randomNum, nextStdGen)

test2 :: State ([a], Char) a
test2 = state $ do
  xs <- fst
  ch <- snd
  return (head xs, (drop 1 xs, succ ch))
