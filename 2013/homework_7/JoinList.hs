{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized

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
(+++) l1 l2 = Append (tag l1 `mappend` tag l2) l1 l2


------------------- Exercise 2 -------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) = if i == 0 then Just a else Nothing
indexJ i (Append m l1 l2)
  | i < 0 || i > size' m = Nothing
  | i < tagSize l1       = indexJ (tagSize l1 - i) l1
  | otherwise            = indexJ (i - tagSize l1) l2
    where size'   = getSize . size
          tagSize = size' . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 1 (Single _ _) = Empty
dropJ i (Append m l1 l2)
  | i == size' m    = Empty
  | i <= tagSize l1 = dropJ i l1 +++ l2
  | otherwise       = dropJ (i - tagSize l1) l2
    where size'   = getSize . size
          tagSize = size' . tag
dropJ i l = if i < 1 then l else undefined

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 1 l@(Single _ _) = l
takeJ i (Append m l1 l2)
    | i == size' m    = l1 +++ l2
    | i <= tagSize l1 = takeJ i l1
    | otherwise       = l1 +++ takeJ (i - tagSize l1) l2
      where size'   = getSize . size
            tagSize = size' . tag
takeJ i _ = if i < 1 then Empty else undefined
