module Q11to20ListsContinuedSpec where

import Test.Hspec
import Q11to20ListsContinued

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- https://wiki.haskell.org/99_questions/11_to_20

spec :: Spec
spec = do

  describe "Problem 11: Modified run-length encoding." $ do
    it "returns a list with singles and multiples" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "Problem 12: Decode a run-length encoded list." $ do
    it "returns a decoded list from an encoded list" $ do
      decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

  describe "Problem 13: Run-length encoding of a list (direct solution)." $ do
    it "returns a list with singles and multiples" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "Problem 14: Duplicate the elements of a list." $ do
    it "repeats each element in the list, once" $ do
      dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3]
      dupli "hello world" `shouldBe` "hheelllloo  wwoorrlldd"

  describe "Problem 15: Replicate the elements of a list a given number \
            \times." $ do
      it "repeats each element in the list n times" $ do
        repli [1, 2, 3] 2 `shouldBe` [1,1,2,2,3,3]
        repli "abc" 3`shouldBe` "aaabbbccc"

  describe "Problem 16: Drop every N'th element from a list." $ do
    it "drops every N'th element in the list" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  describe "Problem 17: Split a list into two parts; the length of the first \
           \part is given." $ do
    it "splits a list by the given index" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  describe "Problem 18: Extract a slice from a list." $ do
    it "Extracts a section of the list from the two indices" $ do
      slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

  describe "Problem 19: Rotate a list N places to the left. \
           \Hint: Use the predefined functions length and (++)." $ do
    it "Rotates a list elements by a specified amount" $ do
      rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
      rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

  describe "Problem 20: Remove the K'th element from a list." $ do
    it "Removes and returns Kth element from a list" $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
      removeAt 4 "abcd" `shouldBe` ('d',"abc")
