module Q21to28ListsAgainSpec where

import Test.Hspec
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
