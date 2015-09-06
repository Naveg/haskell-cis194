{-# OPTIONS_GHC -Wall #-}

module HW4 where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

hail :: Integer -> Integer
hail x
  | even x = div x 2
  | otherwise = 3 * x + 1

fun2':: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate hail

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

size :: Tree a -> Integer
size Leaf = 0
size (Node _ l _ r) = 1 + size l + size r

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l d r) =
  if size l <= size r
     then let st = insert x l in Node (height st + 1) st d r
     else let st = insert x r in Node (height st + 1) l d st

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

sieveVals :: Integer -> [Integer]
sieveVals n = filter (<=n) [i+j+2*i*j | i <- [1..n], j <- [i..n]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) . filter (flip notElem $ sieveVals n) $ [1..n]
