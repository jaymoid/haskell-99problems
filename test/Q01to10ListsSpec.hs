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

  describe "Problem 1: Find the last element of a list." $ do
    it "returns the last element in the provided list" $ do
      myLast [1,2,3,4]  `shouldBe` 4
      myLast ['x','y','z'] `shouldBe` 'z'

  describe "Problem 2: Find the last but one element of a list." $ do
    it "returns the last but one element in the list" $ do
      myButLast [1,2,3,4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'

  describe "Problem 3:the K'th element of a list. \
           \The first element in the list is number 1." $ do
    it "returns the kth element in the provided list" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "Problem 4: Find the number of elements of a list." $ do
    it "returns the length of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13
    it "empty list returns 0" $ do
      myLength [] `shouldBe` 0

  describe "Problem 5: Reverse a list." $ do
    it "reverses the list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

  describe "Problem 6: Find out whether a list is a palindrome. A palindrome\
            \can be read forward or backward; e.g. (x a m a x)" $ do
    it "returns true for a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "Problem 7: Flatten a nested list structure.\
           \Transform a list, possibly holding lists as elements into a \
           \`flat' list by replacing each list with its elements \
           \(recursively)." $ do
    it "flattens the list" $ do
      flatten (Elem 5)  `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` [1,2,3,4,5]
    it "empty nested lists returns a empty list" $ do
      flatten (List [] :: NestedList Int ) `shouldBe` ([])

  describe "Problem 8 :Eliminate consecutive duplicates of list elements.\n\
           \If a list contains repeated elements they should be replaced \
           \with a single copy of the element. The order of the elements \
           \should not be changed." $ do
    it "should remove consecutive elements" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "should not change lists without consecutive elements" $ do
      compress "abcade" `shouldBe` "abcade"

  describe "Problem 9: Pack consecutive duplicates of list elements into \
           \sublists. If a list contains repeated elements they should be \
           \placed in separate sublists." $ do
    it "packs consecutives into sublists" $ do
      pack "112233" `shouldBe` ["11","22","33"]
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe`  ["aaaa","b","cc","aa","d","eeee"]
    it "lists with no consecutives are in singleton lists" $ do
      pack "123" `shouldBe` ["1","2","3"]

  describe "Problem 10: Run-length encoding of a list. Use the result \
           \of problem P09 to implement the so-called run-length \
           \encoding data compression method. Consecutive duplicates of \
           \elements are encoded as lists (N E) where N is the number \
           \of duplicates of the element E." $ do
     it "run length encodes the list" $ do
       encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
