{-# OPTIONS_GHC -Wall #-}

module Golf where

everyNth :: [a] -> Int -> [a]
everyNth [] _ = []
everyNth xs n
  | length xs < n = []
  | otherwise = head left : everyNth (drop 1 left) n
    where left = drop (n - 1) xs

skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = if y > x && y > z then y : rest else rest
  where rest = localMaxima (y:z:xs)
localMaxima _ = []

countN :: [Integer] -> Integer -> Int
countN xs n = length (filter (==n) xs)

suffix :: String
suffix = "==========\n0123456789\n"

row :: [Int] -> Int -> String
row xs n = map (\x -> if x >= n then '*' else ' ') xs

histogram :: [Integer] -> String
histogram xs = unlines (map (row counts) [maxc,maxc-1..1]) ++ suffix
  where counts = map (countN xs) [0..9]
        maxc = maximum counts
