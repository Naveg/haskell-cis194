{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import Editor
import Sized
import Buffer
import Data.Monoid

import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty
  mappend = (+++)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m1 _) = m1
tag (Append m1 _ _) = m1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl +++ Empty = jl
Empty +++ jl = jl
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a1) = Just a1
indexJ _ (Single _ _) = Nothing
indexJ n (Append _ jl1 jl2)
  | n < k = indexJ n jl1
  | otherwise = indexJ (n - k) jl2
    where k = jlSize jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ jl1 jl2)
  | n < k = dropJ n jl1 +++ jl2
  | otherwise = dropJ (n - k) jl2
    where k = jlSize jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ s@(Single _ _) = s
takeJ n (Append _ jl1 jl2)
  | n < k = takeJ n jl1
  | n == k = jl1
  | otherwise = jl1 +++ takeJ (n - k) jl2
    where k = jlSize jl1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

toList :: JoinList m a -> [a]
toList Empty = []
toList (Single _ a) = [a]
toList (Append _ jl1 jl2) = toList jl1 ++ toList jl2

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . toList
  fromString = mconcat . map (\l -> Single (scoreString l, 1) l) . lines
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

startingBuf :: JoinList (Score, Size) String
startingBuf = fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main :: IO ()
main = runEditor editor startingBuf
