{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = let match = zipWith (==) a b in
                    length (filter (== True) match)

-- Exercise 2 -----------------------------------------

countColor :: Peg -> Code -> Int
countColor peg code = length (filter (== peg) code)

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors a = map ($ a) (map countColor colors)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum (zipWith min (countColors a) (countColors b))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact nonexact
                where
                exact = exactMatches s g
                nonexact = matches s g - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move g ex nex) code =
    (Move g ex nex) == getMove code g

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n <= 0 = []
    | n == 1 = group colors
    | otherwise = nub (concatMap addColors (allCodes (n-1)))

addColors :: Code -> [Code]
addColors c = concatMap ($ c) (map addColor colors)

addColor :: Peg -> Code -> [Code]
addColor p c = addColorLoc p 0 c

addColorLoc :: Peg -> Int -> Code -> [Code]
addColorLoc p n c
    | n == (length c) = [c ++ [p]]
    | otherwise = [(take n c) ++ [p] ++ (drop n c)] ++ (addColorLoc p (n+1) c)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve s = solveConsistent s (allCodes (length s))

solveConsistent :: Code -> [Code] -> [Move]
solveConsistent _ [] = []
solveConsistent s (c:cs) = [thisMove] ++ nextMoves
    where
    thisMove = getMove s c
    nextMoves = if (moveExact thisMove) == (length s)
                then []
                else solveConsistent s (filterCodes thisMove cs)

moveExact :: Move -> Int
moveExact (Move _ ex _) = ex

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
