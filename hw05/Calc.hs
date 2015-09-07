{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as Map

import ExprT
import Parser
import qualified StackVM

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Nothing -> Nothing
              Just e -> Just $ eval e

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit x = x
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit x = x > 0
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

maybeOp :: (Num a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeOp _ Nothing _ = Nothing
maybeOp _ _ Nothing = Nothing
maybeOp op (Just a) (Just b) = Just $ op a b

instance Expr (Map.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add a b = \m -> maybeOp (+) (a m) (b m)
  mul a b = \m -> maybeOp (*) (a m) (b m)

instance HasVars (Map.Map String Integer -> Maybe Integer) where
  var = Map.lookup
