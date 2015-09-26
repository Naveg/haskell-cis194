{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.List

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (empFun e + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a $ map (treeFold f) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e subLists = (bestWith, bestWithout)
  where bestWith = glCons e (mconcat (map snd subLists))
        bestWithout = mconcat (map (uncurry moreFun) subLists)

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t

main :: IO ()
main = do
  contents <- readFile "company.txt"
  let (GL es fun) = maxFun (read contents :: Tree Employee) in do
    putStrLn ("Total fun: " ++ show fun)
    putStrLn (init . unlines . sort $ map empName es)
