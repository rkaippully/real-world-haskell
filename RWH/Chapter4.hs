{- |
Module      : RWH.Chapter4
Description : Solutions for exercises from Chapter 4
Copyright   : Raghu Kaippully, 2015
License     : GPL Version 3
-} 

module RWH.Chapter4 where

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
                                then (e, (dropWhile f ys))
                                else accumulateWhile (e ++ [y], ys)
  in
   if null rest then [elm] else [elm] ++ splitWith f rest

