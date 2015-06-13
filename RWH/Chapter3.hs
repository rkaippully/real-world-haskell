{- |
Module      : RWH.Chapter3
Description : Solutions for exercises from Chapter 3
Copyright   : Raghu Kaippully, 2015
License     : GPL Version 3
-} 

module RWH.Chapter3 where

import Data.List (sortBy, minimumBy)
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

{- |
    Using the binary tree type that we defined earlier in this chapter, write a function
    that will determine the height of the tree. The height is the largest number of hops
    from the root to an Empty. For example, the tree Empty has height zero; Node "x"
    Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height
    two; and so on.
-}
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)

{- |
    Consider three two-dimensional points, a, b, and c. If we look at the angle formed
    by the line segment from a to b and the line segment from b to c, it turns left, turns
    right, or forms a straight line. Define a Direction data type that lets you represent
    these possibilities.
-}
data Direction = LeftTurn | RightTurn | StraightLine deriving (Eq, Show)

{- |
    Write a function that calculates the turn made by three two-dimensional points
    and returns a Direction.
-}
-- This can easily be solved using vector cross product. Find the cross product
-- of the vector AB with the vector BC. If that is positive, it is a left turn.
-- If it is negative, it is a right turn. If it is zero, it is a straight line.
data Point = Point { px :: Double, py :: Double } deriving (Eq)

instance Show Point where
  show p = show (px p, py p)

turnDirection :: Point -> Point -> Point -> Direction
turnDirection a b c
  | crossProduct < 0 = RightTurn
  | crossProduct > 0 = LeftTurn
  | otherwise = StraightLine
  where
    sub p1 p2 = Point {px = px p1 - px p2, py = py p1 - py p2}
    p = b `sub` a
    q = c `sub` b
    crossProduct = px p * py q - py p * px q

{- |
    Define a function that takes a list of two-dimensional points and computes the
    direction of each successive triple. Given a list of points [a,b,c,d,e], it should
    begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
    then [c,d,e]. Your function should return a list of Direction.
-}
turnDirections :: [Point] -> [Direction]
turnDirections (a:b:c:xs) = turnDirection a b c : turnDirections (b:c:xs)
turnDirections _ = []

{- |
    Using the code from the preceding three exercises, implement Graham’s scan algorithm
    for the convex hull of a set of 2D points. You can find good description
    of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the
    Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work,
    on Wikipedia (http://en.wikipedia.org/).
-}
grahamScan :: [Point] -> [Point]
grahamScan ps =
  case scan ((sortBy angleOf ps) ++ [lowestPoint]) of
   Just path -> path
   Nothing -> error "grahamScan failed"
  where
    -- Lower point by y co-ordinate, use x co-ordinate if y is equal
    lowestPoint = minimumBy (\p q -> compare (py p, px p) (py q, px q)) ps

    -- Sort two points by the angle they and the lowestPoint makes with x-axis
    angleOf p q = compare (arctan p) (arctan q)
    arctan p = atan2 (py p - py lowestPoint) (px p - px lowestPoint)

    -- graham scanning
    scan :: [Point] -> Maybe [Point]
    scan (p:q:r:xs) =
      case turnDirection p q r of
       LeftTurn -> case scan (q:r:xs) of
                    Just path -> Just (p:path)
                    Nothing -> scan (p:q:xs)
       RightTurn -> Nothing
       StraightLine -> scan (p:r:xs)
    scan [p, q] = Just [p, q]
    scan _ = Nothing
