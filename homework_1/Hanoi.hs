module Hanoi(hanoi, hanoi4) where

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0    = []
  | otherwise = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a

hanoi4 :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n <= 0    = []
  | n == 1    = [(a,b)]
  | otherwise = hanoi4 m a c b d ++ hanoi4 (n - m) a b d c ++ hanoi4 m c b a d
    where m = n `div` 2
