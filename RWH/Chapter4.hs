{- |
Module      : RWH.Chapter4
Description : Solutions for exercises from Chapter 4
Copyright   : Raghu Kaippully, 2015
License     : GPL Version 3
-} 

module RWH.Chapter4 where

import System.Environment (getArgs)
import Data.List (foldl')
import Data.Char

{- |
    Write your own “safe” definitions of the standard partial list functions, but make
    sure they never fail. As a hint, you might want to consider using the following types:
    -- file: ch04/ch04.exercises.hs
    safeHead :: [a] -> Maybe a
    safeTail :: [a] -> Maybe [a]
    safeLast :: [a] -> Maybe a
    safeInit :: [a] -> Maybe [a]
-}
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs
safeLast [] = Nothing

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (allExceptLast xs)
  where
    allExceptLast (_:[]) = []
    allExceptLast (y:ys) = y : allExceptLast ys
    allExceptLast [] = error "This will never happen, just to avoid a compiler warning"

{- |
    Write a function splitWith that acts similarly to words but takes a predicate and a
    list of any type, and then splits its input list on every element for which the predicate
    returns False:
    -- file: ch04/ch04.exercises.hs
    splitWith :: (a -> Bool) -> [a] -> [[a]]
-}
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs =
  let
    (elm, rest) = accumulateWhile ([], xs)
    accumulateWhile (e, []) = (e, [])
    accumulateWhile (e, y:ys) = if f y
                                then (e, dropWhile f ys)
                                else accumulateWhile (e ++ [y], ys)
  in
   if null rest then [elm] else elm : splitWith f rest

{- |
    Using the command framework from the earlier section “A Simple Command-Line
    Framework” on page 71, write a program that prints the first word of each line of
    its input.
-}
interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

printFirstWord :: IO ()
printFirstWord = mainWith getFirstWord
  where mainWith function = do
          args <- getArgs
          case args of
           [input, output] -> interactWith function input output
           _ -> putStrLn "error: exactly two arguments needed"

getFirstWord :: String -> String
getFirstWord text = unlines $ map (head . words) $ lines text

{- |
    Write a program that transposes the text in a file. For instance, it should convert
    "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
-}
transposeText :: FilePath -> FilePath -> IO ()
transposeText input output = do
  let
    transpose :: [String] -> [String]
    transpose [] = []
    transpose (x:xs) = case x of
      [] -> []
      (_:[]) -> [map head (x:xs)]
      _ -> map head (x:xs) : transpose (map tail (x:xs))

  text <- readFile input
  writeFile output $ unlines $ transpose $ lines text

{- |
    Use a fold (choosing the appropriate fold will make your code much simpler) to
    rewrite and improve upon the asInt function from the earlier section“Explicit Recursion”
    on page 85.

    Your function should behave as follows:
    ghci> asInt' "101"
    101
    ghci> asInt' "-31337"
    -31337
    ghci> asInt' "1798"
    1798

    Extend your function to handle the following kinds of exceptional conditions by
    calling error:
    ghci> asInt' ""
    0
    ghci> asInt' "-"
    0
    ghci> asInt' "-3"
    -3
    ghci> asInt' "2.7"
    *** Exception: Char.digitToInt: not a digit '.'
    ghci> asInt' "314159265358979323846"
    564616105916946374
-}
asInt' :: String -> Int
asInt' [] = 0
asInt' s@(c:cs) =
  if c == '-'
  then -1 * foldl' collect 0 cs
  else foldl' collect 0 s
  where
    collect val ch = val*10 + digitToInt ch

{- |
    The asInt' function uses error, so its callers cannot handle errors. Rewrite
    the function to fix this problem:
    -- file: ch04/ch04.exercises.hs
    type ErrorMessage = String
    asIntEither :: String -> Either ErrorMessage Int
    ghci> asIntEither "33"
    Right 33
    ghci> asIntEither "foo"
    Left "non-digit 'o'"
-}
type ErrorMessage = String

asIntEither :: String -> Either ErrorMessage Int
asIntEither [] = Right 0
asIntEither s@(c:cs) =
  if c == '-'
  then
    let
      val = foldl' collect (Right 0) cs
    in
     case val of
      err@(Left _) -> err
      Right v -> Right (-1 * v)
  else foldl' collect (Right 0) s
  where
    collect err@(Left _) _ = err
    collect (Right val) ch = if isDigit ch
                             then Right (val*10 + digitToInt ch)
                             else Left ("non-digit " ++ show ch)

{- |
    The Prelude function concat concatenates a list of lists into a single list and has the
    following type:
    -- file: ch04/ch04.exercises.hs
    concat :: [[a]] -> [a]
    Write your own definition of concat using foldr.
-}
concat' :: [[a]] -> [a]
concat' = foldr (++) []

{- |
    Write your own definition of the standard takeWhile function, first using explicit
    recursion, and then foldr.
-}
takeWhileRecurse :: (a -> Bool) -> [a] -> [a]
takeWhileRecurse _ [] = []
takeWhileRecurse f (x:xs) = if f x then x:takeWhileRecurse f xs else []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f = foldr collect []
  where
    collect x acc = if f x then x:acc else []

{- |
    The Data.List module defines a function, groupBy, which has the following type:
    -- file: ch04/ch04.exercises.hs
    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    Use ghci to load the Data.List module and figure out what groupBy does, then
    write your own implementation using a fold.
-}
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr collect []
  where
    collect x [] = [[x]]
    collect x acc@(y:ys)
      | f x (head y) = (x:y):ys
      | otherwise = [x]:acc

{- |
    How many of the following Prelude functions can you rewrite using list folds?
      . any
      . cycle
      . words
      . unlines
    For those functions where you can use either foldl' or foldr, which is more appropriate
    in each case?
-}
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x y -> y || f x) False  

cycle' :: [a] -> [a]
cycle' xs = foldr (++) [] (repeat xs)

words' :: String -> [String]
words' [] = []
words' str = filter (not . null) $ foldr collect [] str
  where
    collect ch [] = [[ch]]
    collect ch acc@(x:xs)
      | isSpace ch = []:acc
      | otherwise = (ch:x):xs

unlines' :: [String] -> String
unlines' = foldr (\ln acc -> ln ++ "\n" ++ acc) ""
