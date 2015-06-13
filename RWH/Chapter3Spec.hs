{- |
   Unit tests for all problems in Chapter 3
-}

module Main where

import Test.Hspec

import RWH.Chapter3

main :: IO ()
main = hspec $ do

  describe "Exercise 1 and 2" $ do
    it "returns 0 for an empty list" $ do
      elementCount [] `shouldBe` 0

    it "returns the currect count for a non-empty list" $ do
      elementCount ['a', 'b', 'c'] `shouldBe` 3


  describe "Exercise 3" $ do
    it "returns 0 for empty list" $ do
      mean ([] :: [Float]) `shouldBe` 0.0

    it "returns 2.5 for [1, 2, 3, 4]" $ do
      mean ([1.0, 2.0, 3.0, 4.0] :: [Float]) `shouldBe` 2.5


  describe "Exercise 4" $ do
    it "returns empty list for an empty list" $ do
      palindrome ([] :: [Int]) `shouldBe` []

    it "works for non-empty list" $ do
      palindrome ([1, 2, 3] :: [Int]) `shouldBe` [1, 2, 3, 3, 2, 1]


  describe "Exercise 5" $ do
    it "returns True for empty list" $ do
      isPalindrome ("" :: String) `shouldBe` True
    
    it "returns False for a non-palindrome list" $ do
      isPalindrome ([1, 2, 3] :: [Int]) `shouldBe` False

    it "returns True for a palindrome list" $ do
      isPalindrome ("racecar" :: String) `shouldBe` True

  describe "Exercise 6" $ do
    it "returns empty list for empty list" $ do
      sortBySubListLength ([] :: [[Int]]) `shouldBe` ([] :: [[Int]])

    it "returns sorted list for a non-empty list" $ do
      sortBySubListLength ([[1,2,3], [1,2], [1]] :: [[Int]]) `shouldBe` ([[1], [1,2], [1,2,3]] :: [[Int]])

  describe "Exercises 7 & 8" $ do
    it "returns empty list for empty list" $ do
      intersperse ',' [] `shouldBe` ""

    it "returns same string for a single element list" $ do
      intersperse ',' ["foo"] `shouldBe` "foo"

    it "adds the separator for larger lists" $ do
      intersperse ',' ["foo", "bar", "baz", "quux"] `shouldBe` "foo,bar,baz,quux"

  describe "Exercise 9" $ do
    it "returns 0 for an empty tree" $ do
      height Empty `shouldBe` 0

    it "returns 1 for single node tree" $ do
      height (Node 'x' Empty Empty) `shouldBe` 1

    it "returns 2 for another tree" $ do
      height (Node 'x' Empty (Node 'y' Empty Empty)) `shouldBe` 2

  describe "Exercise 11" $ do
    it "returns LeftTurn for negative x axis and positive y axis" $ do
      turnDirection Point{px = -1, py = 0} Point{px = 0, py = 0} Point{px = 0, py = 1} `shouldBe` LeftTurn

    it "returns RightTurn for negative y axis and positive x axis" $ do
      turnDirection Point{px = 0, py = -1} Point{px = 0, py = 0} Point{px = 1, py = 0} `shouldBe` RightTurn

    it "returns LeftTurn for negative y axis and negative x axis" $ do
      turnDirection Point{px = 0, py = -1} Point{px = 0, py = 0} Point{px = -1, py = 0} `shouldBe` LeftTurn

    it "returns StraightLine for points on y axis" $ do
      turnDirection Point{px = 0, py = -1} Point{px = 0, py = 0} Point{px = 0, py = 5} `shouldBe` StraightLine

  describe "Exercise 12" $ do
    let
      a = Point 2 4
      b = Point 3 5
      c = Point 3 3
      d = Point 4 6
      e = Point 4 4
      f = Point 4 2
      g = Point 5 5
      h = Point 5 3
      i = Point 6 4
      j = Point (-2) 3
      k = Point 6 (-2)
      l = Point 6 6
    
    it "solves problem 1" $ do
      grahamScan [a, b, c, d, e, f, g, h, i] `shouldBe`[f, i, d, a, f]

    it "solves problem 2" $ do
      grahamScan [a, b, c, d, e, f, g, h, i, j, k, l] `shouldBe` [k, l, d, j, k]
