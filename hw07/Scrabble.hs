{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score a) = a

instance Monoid Score where
  mempty = 0
  mappend = (+)

score :: Char -> Score
score c
  | c' `elem` "EAIONRTLSU" = 1
  | c' `elem` "DG"         = 2
  | c' `elem` "BCMP"       = 3
  | c' `elem` "FHVWY"      = 4
  | c' `elem` "K"          = 5
  | c' `elem` "JX"         = 8
  | c' `elem` "QZ"         = 10
  | otherwise              = 0
  where c' = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score
