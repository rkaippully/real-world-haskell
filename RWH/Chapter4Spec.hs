{- |
   Unit tests for all problems in Chapter 4
-}

module Main where

import Data.Char
import Test.Hspec

import RWH.Chapter4

main :: IO ()
main = hspec $ do

  describe "Exercise 1" $ do
    it "safeHead returns Nothing for an empty list" $ do
      safeHead (""::String) `shouldBe` Nothing

    it "safeHead returns head of the non-empty list" $ do
      safeHead (repeat 'a') `shouldBe` Just 'a'

    it "safeTail returns Nothing for an empty list" $ do
      safeTail (""::String) `shouldBe` Nothing

    it "safeTail returns an empty list for a single element list" $ do
      safeTail ("a"::String) `shouldBe` Just []

    it "safeTail returns all except head for a non-empty list" $ do
      safeTail ("abcd"::String) `shouldBe` Just ("bcd"::String)

    it "safeLast returns Nothing for an empty list" $ do
      safeLast (""::String) `shouldBe` Nothing

    it "safeLast returns the last element for non-empty list" $ do
      safeLast ("abcd"::String) `shouldBe` Just 'd'

    it "safeInit returns Nothing for an empty list" $ do
      safeInit (""::String) `shouldBe` Nothing

    it "safeInit returns a single element list for a size two list" $ do
      safeInit ("ab"::String) `shouldBe` Just ("a"::String)

    it "safeInit returns all but the last element for non-empty list" $ do
      safeInit ("abcd"::String) `shouldBe` Just ("abc"::String)

    it "splitWith returns empty list for empty list" $ do
      splitWith isSpace ("Mary had a\t\tlittle lamb"::String) `shouldBe` ["Mary", "had", "a", "little", "lamb"]

    it "transposes text in a file" $ do
      transposeText "test-data/transpose-input.txt" "test-data/transpose-output.txt"
      text <- readFile "test-data/transpose-output.txt"
      text `shouldBe` "hw\neo\nlr\nll\nod\n"
