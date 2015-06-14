{- |
   Unit tests for all problems in Chapter 4
-}

module Main where

import Data.Char
import Test.Hspec
import Control.Exception

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

    it "asInt' should handle various cases" $ do
      asInt' "101" `shouldBe` 101
      asInt' "-31337" `shouldBe` -31337
      asInt' "1798" `shouldBe` 1798
      asInt' "" `shouldBe` 0
      asInt' "-" `shouldBe` 0
      asInt' "-3" `shouldBe` -3
      evaluate (asInt' "2.7") `shouldThrow` errorCall "Char.digitToInt: not a digit '.'"
      asInt' "314159265358979323846" `shouldBe` 564616105916946374

    it "asIntEither should handle various cases" $ do
      asIntEither "101" `shouldBe` Right 101
      asIntEither "-31337" `shouldBe` Right (-31337)
      asIntEither "1798" `shouldBe` Right 1798
      asIntEither "" `shouldBe` Right 0
      asIntEither "-" `shouldBe` Right 0
      asIntEither "-3" `shouldBe` Right (-3)
      asIntEither "2.7" `shouldBe` Left "non-digit '.'"
      asIntEither "314159265358979323846" `shouldBe` Right 564616105916946374

    it "concat' should return empty list for an empty list" $ do
      concat' ([] :: [String]) `shouldBe` []

    it "concat' should return the concatenated list for other inputs" $ do
      concat' (["Mary", " had", " a", " little", " lamb"] :: [String]) `shouldBe` "Mary had a little lamb"

    it "takeWhileRecurse on empty list returns empty list" $ do
      takeWhileRecurse (== 'a') (""::String) `shouldBe` ""

    it "takeWhileRecurse on a non-empty list" $ do
      takeWhileRecurse (< 10) ([1, 1, 2, 3, 5, 8, 13, 21, 34]::[Int]) `shouldBe` ([1, 1, 2, 3, 5, 8]::[Int])

    it "takeWhileFoldr on empty list returns empty list" $ do
      takeWhileFoldr (== 'a') (""::String) `shouldBe` ""

    it "takeWhileFoldr on a non-empty list" $ do
      takeWhileFoldr (< 10) ([1, 1, 2, 3, 5, 8, 13, 21, 34]::[Int]) `shouldBe` ([1, 1, 2, 3, 5, 8]::[Int])

    it "groupBy' groups Mississippi correctly" $ do
      groupBy' (==) ("Mississippi" :: String) `shouldBe` (["M","i","ss","i","ss","i","pp","i"] :: [String])

    it "any' tests" $ do
      any' even ([1, 2, 3, 4] :: [Int]) `shouldBe` True
      any' odd ([2, 4, 6, 8] :: [Int]) `shouldBe` False
      any' odd ([] :: [Int]) `shouldBe` False

    it "cycle' tests" $ do
      take 10 (cycle' ([1, 2, 3] :: [Int])) `shouldBe` ([1, 2, 3, 1, 2, 3, 1, 2, 3, 1] :: [Int])

    it "words' tests" $ do
      words' "Mary  had \t a little lamb" `shouldBe` ["Mary", "had", "a", "little", "lamb"]

    it "unlines' tests" $ do
      unlines' ["Twinkle", "twinkle", "little", "star"] `shouldBe` "Twinkle\ntwinkle\nlittle\nstar\n"
