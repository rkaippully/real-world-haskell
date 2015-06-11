{- |
   Unit tests for all problems in Chapter 3
-}

module RWH.Chapter3Spec where

import Test.Hspec

import RWH.Chapter3

chapter3Specs :: IO ()
chapter3Specs = hspec $ do

  describe "Chapter 3 exercise 1 and 2" $ do

    it "returns 0 for an empty list" $ do
      elementCount [] `shouldBe` 0

    it "returns the currect count for a non-empty list" $ do
      elementCount ['a', 'b', 'c'] `shouldBe` 3

  describe "Chapter 3 exercise 3" $ do

    it "returns empty list for an empty list" $ do
      palindrome ([] :: [Int]) `shouldBe` []

    it "works for non-empty list" $ do
      palindrome ([1, 2, 3] :: [Int]) `shouldBe` [1, 2, 3, 3, 2, 1]
