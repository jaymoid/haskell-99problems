module Q01to10ListsSpec where

import Test.Hspec
import Q01to10Lists

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- https://wiki.haskell.org/99_questions/1_to_10

spec :: Spec
spec = do

  describe "Problem 1" $ do
    it "returns the last element in the provided list" $ do
      myLast [1,2,3,4]  `shouldBe` 4
      myLast ['x','y','z'] `shouldBe` 'z'

  describe "Problem 2" $ do
    it "returns the last but one element in the list" $ do
      myButLast [1,2,3,4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'

  describe "Problem 3" $ do
    it "returns the kth element in the provided list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "Problem 4" $ do
    it "returns the length of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13
    it "empty list returns 0" $ do
      myLength [] `shouldBe` 0

  describe "Problem 5" $ do
    it "reverses the list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

  describe "Problem 6" $ do
    it "returns true for a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "Problem 7" $ do
    it "flattens the list" $ do
      flatten (Elem 5)  `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` [1,2,3,4,5]
    it "empty nested lists returns a empty list" $ do
      flatten (List [] :: NestedList Int ) `shouldBe` ([])

  describe "Problem 8" $ do
    it "should remove consecutive elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "should not change lists without consecutive elements" $ do
      compress "abcade" `shouldBe` "abcade"

  describe "Problem 9" $ do
    it "packs consecutives into sublists" $ do
      pack "112233" `shouldBe` ["11","22","33"]
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe`  ["aaaa","b","cc","aa","d","eeee"]
    it "lists with no consecutives are in singleton lists" $ do
      pack "123" `shouldBe` ["1","2","3"]

  describe "Problem 10" $ do
     it "run length encodes the list" $ do
       encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
