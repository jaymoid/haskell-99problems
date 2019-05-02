module Q21to28ListsAgainSpec where

import Test.Hspec
import Data.List (sort)
import Data.Set (Set(), fromList, empty, size)
import Q21to28ListsAgain

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- https://wiki.haskell.org/99_questions/21_to_21

spec :: Spec
spec = do

  describe "Problem 21" $ do
    it "can insert an element at a given position into a list." $ do
      insertAt 'X' "abcd" 2  `shouldBe` "aXbcd"

  describe "Problem 22" $ do
    it "creates a list containing all integers within a given range." $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]

  describe "Problem 23" $ do
    it "extract a given number of randomly selected elements from a list." $ do
      let input = "abcdefgh"
          randomElements = rndSelect input 3
      randomElements `shouldSatisfy` all (flip elem input)

  describe "Problem 24" $ do
    it "draw N different random numbers from the set 1..M. " $ do
      diffSelect 6 49 `shouldSatisfy` (\xs -> all (flip elem [1..49]) xs && length xs == 6)

  describe "Problem 25" $ do
    it "generate a random permutation of the elements of a list." $ do
      let originalList = "badcef"
      rndPermu originalList `shouldSatisfy` (\xs ->
          length originalList == length xs
          &&
          sort originalList == sort xs
          &&
          (originalList /= xs)
        )

  describe "Problem 26" $ do
    it "generates the combinations of K distinct objects chosen from the N elements of a list" $ do
      let set :: (Ord a) => [[a]] -> Set (Set a)
          set xs =  fromList $ fmap fromList xs
      combinations 0 "abc" `shouldBe` empty
      combinations 1 "abc" `shouldBe` set ["a", "b", "c"]
      combinations 2 "abc" `shouldBe` set ["cb", "ab", "ca"]
      combinations 3 "abc" `shouldBe` set ["cb", "ab", "ca"]
      combinations 3 "abc" `shouldBe` set [
          "cba", "dba", "eba",
          "dca", "eca", "eda",
          "dcb", "ecb", "edb",
          "edc"]
      size (combinations 3 "abcdefghijkl") `shouldBe` 220
