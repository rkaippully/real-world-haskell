{- |
Module      : RWH.Chapter3
Description : Solutions for exercises from Chapter 3
Copyright   : Raghu Kaippully, 2015
License     : GPL Version 3
-} 

module RWH.Chapter3 where

import Data.List (sortBy)
import Data.Function

{- |
   Write a function that computes the number of elements in a list. To test it, ensure
   that it gives the same answers as the standard length function.
   Add a type signature for your function to your source file. To test it, load the source
   file into ghci again.
-}
elementCount :: [a] -> Int
elementCount = foldr count 0
  where count _ c = c + 1

{- |
  Write a function that computes the mean of a list, i.e., the sum of all elements in
  the list divided by its length. (You may need to use the fromIntegral function to
  convert the length of the list from an integer into a floating-point number.)
-}
mean :: Fractional a => [a] -> a
mean [] = 0.0
mean xs = total / count
  where
    (total, count) = foldr totalAndCount (0.0, 0) xs
    totalAndCount x (t, c) = (t + x, c + 1)

{- |
  Turn a list into a palindrome; i.e., it should read the same both backward and
  forward. For example, given the list [1,2,3], your function should return
  [1,2,3,3,2,1].
-}
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:[]) = [x, x]
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]

{- |
  Write a function that determines whether its input list is a palindrome.
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

{- |
  Create a function that sorts a list of lists based on the length of each sublist. (You
  may want to look at the sortBy function from the Data.List module.)
-}
sortBySubListLength :: [[a]] -> [[a]]
sortBySubListLength = sortBy (compare `on` length)

{- |
  Define a function that joins a list of lists together using a separator value:
  -- file: ch03/Intersperse.hs
  intersperse :: a -> [[a]] -> [a]
   The separator should appear between elements of the list, but it should not follow
  the last element. Your function should behave as follows:

  ghci> :load Intersperse
  [1 of 1] Compiling Main ( Intersperse.hs, interpreted )
  Ok, modules loaded: Main.
  ghci> intersperse ',' []
  ""
  ghci> intersperse ',' ["foo"]
  "foo"
  ghci> intersperse ',' ["foo","bar","baz","quux"]
  "foo,bar,baz,quux"
-}
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs