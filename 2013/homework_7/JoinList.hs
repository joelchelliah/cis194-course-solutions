{-# LANGUAGE FlexibleInstances #-} -- Exercise 4
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid
import Sized    -- Exercise 2
import Scrabble -- Exercise 3
import Buffer   -- Exercise 4
import Editor   -- Exercise 4


------------------- Exercise 1 -------------------

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _)   = m
tag Empty          = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2


------------------- Exercise 2 -------------------

tagSize :: (Monoid b, Sized b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i l
  | i < 0         = Nothing
  | i > tagSize l = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ l1 l2)
  | i < tagSize l1 = indexJ i l1
  | otherwise      = indexJ (i - tagSize l1) l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l
  | i <= 0        = l
  | i > tagSize l = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append _ l1 l2)
  | i < tagSize l1  = dropJ i l1 +++ l2
  | otherwise       = dropJ (i - tagSize l1) l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i l
  | i <= 0        = Empty
  | i > tagSize l = l
takeJ _ l@(Single _ _) = l
takeJ i (Append _ l1 l2)
    | i < tagSize l1 = takeJ i l1
    | otherwise      = l1 +++ takeJ (i - tagSize l1) l2


------------------- Exercise 3 -------------------

scoreLine :: String -> JoinList Score String
scoreLine s  = Single (scoreString s) s


------------------- Exercise 4 -------------------

instance Buffer (JoinList (Score, Size) String) where
  toString (Append _ l1 l2) = toString l1 ++ toString l2
  toString (Single _ s)     = s
  toString Empty            = ""

  fromString = fromLines . lines
    where fromLines      = foldr ((+++) . scoreAndSize) Empty
          scoreAndSize s = Single (scoreString s, Size 1) s

  line = indexJ

  replaceLine i s l = takeJ i l +++ fromString s +++ dropJ (i + 1) l

  numLines = tagSize

  value = getScore . fst . tag


main :: IO ()
main = let asJoinList s = fromString s :: JoinList (Score, Size) String
       in runEditor editor . asJoinList $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
