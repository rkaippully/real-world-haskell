
module Main where


import Test.Hspec

import RWH.Chapter3

main :: IO ()
main = hspec $ do

  describe "Chapter3 exercise 1 and 2" $ do
    it "number of elements in a list" $ do
      elementCount [] `shouldBe` 0
