{-# OPTIONS_GHC -Wall #-}

module HW1Sol where

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
  | n < 10 = [n]
  | otherwise = mod n 10 : toDigitsRev (div n 10)

toDigits n = reverse (toDigitsRev n)

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft [x] = [x]
doubleEveryOtherLeft (x : y : xs) = x : 2 * y : doubleEveryOtherLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherLeft (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

-- Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
