{-# LANGUAGE TypeSynonymInstances #-} -- Exercise 5
{-# LANGUAGE FlexibleInstances    #-} -- Exercise 5 & 6
{-# OPTIONS_GHC -Wall #-}
module Calc where

import           ExprT
import           Parser
import qualified StackVM  as Stack -- Exercise 5
import qualified Data.Map as M     -- Exercise 6

------------------- Exercise 1 -------------------

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


------------------- Exercise 2 -------------------

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


------------------- Exercise 3 -------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul


------------------- Exercise 4 -------------------

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  (MinMax x) `add` (MinMax y) = lit $ x `max` y
  (MinMax x) `mul` (MinMax y) = lit $ x `min` y

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  (Mod7 x) `add` (Mod7 y) = lit $ x + y
  (Mod7 x) `mul` (Mod7 y) = lit $ x * y


------------------- Exercise 5 -------------------

instance Expr Stack.Program where
  lit e = [Stack.PushI e]
  e1 `add` e2 = e1 ++ e2 ++ [Stack.Add]
  e1 `mul` e2 = e1 ++ e2 ++ [Stack.Mul]

compile :: String -> Maybe Stack.Program
compile = parseExp lit add mul


------------------- Exercise 6 -------------------

class HasVars a where
  var :: String -> a

data VarExprT = Var String
              | VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  v1 `add` v2 = \m -> (+) <$> v1 m <*> v2 m
  v1 `mul` v2 = \m -> (*) <$> v1 m <*> v2 m
