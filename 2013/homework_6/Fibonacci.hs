{-# LANGUAGE FlexibleInstances            #-} -- Exercise 6
{-# OPTIONS_GHC -fno-warn-missing-methods #-} -- Exercise 6 & 7
{-# OPTIONS_GHC -Wall                     #-}
module Fibonacci where

------------------- Exercise 1 -------------------

fib :: Integer -> Integer
fib n
  | n `elem` [0,1] = n
  | n >= 2         = fib (n - 1) + fib (n - 2)
  | otherwise      = undefined

fibs1 :: [Integer]
fibs1 = fib <$> [0..]


------------------- Exercise 2 -------------------

fibs2 :: [Integer]
fibs2 = fib' 0 1
  where fib' n1 n2 = n1 : fib' n2 (n1 + n2)


------------------- Exercise 3 -------------------

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a as) = a : streamToList as

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


------------------- Exercise 4 -------------------

streamRepeat :: a -> Stream a
streamRepeat a = Stream a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a as) = Stream (f a) $ streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a . streamFromSeed f $ f a


------------------- Exercise 5 -------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr1 interleave $ streamRepeat <$> [0..]
  where interleave (Stream a as) bs = Stream a $ interleave bs as


------------------- Exercise 6 -------------------

x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger i = Stream i $ streamRepeat 0
  negate = streamMap negate
  signum = undefined
  abs = undefined
  (Stream a0 a') + (Stream b0 b') = Stream (a0 + b0) $ a' + b'
  (Stream a0 a') * (Stream b0 b') = Stream (a0 * b0) $ a0b' + a'b
    where a0b' = streamMap (*a0) b'
          a'b  = a' * Stream b0 b'

instance Fractional (Stream Integer) where
  fromRational = undefined
  (Stream a0 a') / (Stream b0 b') = q
    where q = Stream (a0 `div` b0) $ streamMap (`div` b0) (a' - q * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)


------------------- Exercise 7 -------------------

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  fromInteger = undefined
  negate = undefined
  signum = undefined
  abs = undefined
  (+) = undefined
  (Matrix a0 b0 c0 d0) * (Matrix a1 b1 c1 d1) = mult
    where mult = Matrix (a0 * a1 + b0 * c1) (a0 * b1 + b0 * d1)
                        (c0 * a1 + d0 * c1) (c0 * b1 + d0 * d1)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn $ f^n
  where f = Matrix 1 1 1 0
        fn (Matrix _ _ a _) = a
