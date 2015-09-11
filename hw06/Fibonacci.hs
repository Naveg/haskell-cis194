{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b, a+b)) (0,1)

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
  show = show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = let b = f a in
                         Cons a (streamFromSeed f b)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Stream of streams for natural numbers
natStreams :: Stream (Stream Integer)
natStreams = streamFromSeed (streamMap (+1)) (streamRepeat 0)

interleave :: Stream a -> Stream a -> Stream a
interleave (Cons a1 s1) s2 = Cons a1 (interleave s2 s1)

ruler :: Stream Integer
ruler = interleave (streamRepeat 0) (streamMap (+1) ruler)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger = flip Cons $ streamRepeat 0
  negate = streamMap (*(-1))
  (Cons a sa) + (Cons b sb) = Cons (a + b) (sa + sb)
  (Cons a0 as) * b@(Cons b0 bs) = Cons (a0 * b0) $ streamMap (*a0) bs + (as * b)

instance Fractional (Stream Integer) where
  a@(Cons a0 as) / b@(Cons b0 bs) = Cons (a0 `div` b0) $ streamMap (`div` b0) as - bs * (a / b)

fibs3 :: Stream Integer
fibs3 = x / (1- x - x*x)

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show)
diag :: Matrix -> Integer
diag (Matrix _ b _ _) = b

instance Num Matrix where
  (Matrix a b c d) * (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fm :: Matrix
fm = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = diag $ fm^n
