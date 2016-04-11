{-# OPTIONS_GHC -Wall #-}
module HigherOrder where

------------------- Exercise 1 -------------------

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1


------------------- Exercise 2 -------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr mkTree Leaf
   where mkTree x Leaf = Node 0 Leaf x Leaf
         mkTree x (Node _ left val right)
          | kids left <= kids right = Node (len left) (mkTree x left) val right
          | otherwise               = Node (len left) left val (mkTree x right)
         len Leaf = 1 :: Integer
         len (Node i _ _ _) = i + 1
         kids Leaf = 0 :: Integer
         kids (Node _ left _ right) = 1 + kids left + kids right


------------------- Exercise 3 -------------------

xor :: [Bool] -> Bool
xor = foldl (\a b -> not a && b || a && not b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = foldr . flip


------------------- Exercise 4 -------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (2:) $ (+1) . (*2) <$> filter (`notElem` remove) [1..n]
  where remove = [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = 2 : foldr sieve [] [1..n]
  where sieve x
          | x `elem` remove = id
          | otherwise       = (2 * x + 1 :)
        remove = [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]
