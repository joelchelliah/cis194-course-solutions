{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Scrabble where

------------------- Exercise 3 -------------------

newtype Score = Score { getScore :: Int } deriving (Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | c `elem` "AEIOULNSTRaeioulnstr" = Score 1
  | c `elem` "DGdg"                 = Score 2
  | c `elem` "BCMPbcmp"             = Score 3
  | c `elem` "FHVWYfhvwy"           = Score 4
  | c `elem` "Kk"                   = Score 5
  | c `elem` "JXjx"                 = Score 8
  | c `elem` "QZqz"                 = Score 10
  | otherwise                       = Score 0

scoreString :: String -> Score
scoreString = mconcat . fmap score
