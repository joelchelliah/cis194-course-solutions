{-# OPTIONS_GHC -Wall #-}
module HigherOrder where

------------------- Exercise 1 -------------------

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1


------------------- Exercise 2 -------------------
