{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P c1) (P c2)
        | l1 < l2   = (take l1 c2) == c1 && tailZero l1 c2
        | l1 > l2   = (take l2 c1) == c2 && tailZero l2 c1
        | otherwise = c1 == c2
        where
            l1 = length c1
            l2 = length c2
            tailZero n cs = allNum 0 (drop n cs)

allNum :: (Eq a, Num a) => a -> [a] -> Bool
allNum _ [] = False
allNum n cs = null $ filter (/= n) cs

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P coeffs) = showPoly $ reverse coeffs

showPoly :: (Num a, Eq a, Show a) => [a] -> String
showPoly [] = "0"
showPoly (c:[]) = show c
showPoly (c:cs) = showSingle c (length cs) ++ term
    where term = if c == 0
                    then showPoly cs
                    else if allNum 0 cs
                    then ""
                    else " + " ++ showPoly cs

showSingle :: (Num a, Eq a, Show a) => a -> Int -> String
showSingle c p = if c == 0 then "" else case p of
    0 -> show c
    1 -> showCo ++ "x"
    _ -> showCo ++ "x^" ++ show p
    where showCo = if c == 1
                    then "" else
                    if c == (-1) then "-"
                    else show c

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (zipWith (+) ex1 ex2)
    where
    ex1 = extend 0 maxLen a
    ex2 = extend 0 maxLen b
    maxLen = max (length a) (length b)

extend :: Num a => a -> Int -> [a] -> [a]
extend val len cs = cs ++ take extra (repeat val)
    where extra = if len <= length cs
                    then 0 else len - length cs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times pa pb = sum $ shiftMult pa pb

shiftMult :: Num a => Poly a -> Poly a -> [Poly a]
shiftMult (P []) (P _) = [P []]
shiftMult (P _) (P []) = [P []]
shiftMult (P (c:cs)) (P cfs) =
    (P newCo) : shiftMult (P cs) (P (0 : cfs))
    where newCo = map (* c) cfs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P (map (* (-1)) cs)
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) n = sum $ zipWith (*) cs powers
    where
        powers = map (n^) (take (length cs) count)
        count = [0, 1..] :: [Int]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n d = iterate deriv d !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P cs) = P (tail (zipWith (*) mult cs))
        where
            mult = take (length cs) count
            count = iterate (+1) 0
