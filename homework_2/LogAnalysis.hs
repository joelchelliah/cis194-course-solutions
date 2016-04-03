{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

------------------- Exercise 1 -------------------

parse :: String -> [LogMessage]
parse s = parseMessage <$> lines s

parseMessage :: String -> LogMessage
parseMessage s = let parts = words s
                     mType = parseMessageType . take 2 $ parts
                     mTime = parseTimeStamp mType . take 2 . drop 1 $ parts
                     mText = parseMessageText mType . drop 2 $ parts
                 in case mType of
                      Just mType' -> LogMessage mType' mTime mText
                      Nothing     -> Unknown s

parseMessageType :: [String] -> Maybe MessageType
parseMessageType ["I", _] = Just Info
parseMessageType ["W", _] = Just Warning
parseMessageType ["E", i] = Just . Error $ read i
parseMessageType _        = Nothing

parseTimeStamp :: Maybe MessageType -> [String] -> TimeStamp
parseTimeStamp (Just (Error _)) = read . last
parseTimeStamp  _               = read . head

parseMessageText :: Maybe MessageType -> [String] -> String
parseMessageText (Just (Error _)) = unwords . tail
parseMessageText  _               = unwords


------------------- Exercise 2 -------------------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMsg Leaf      = Node Leaf newMsg Leaf
insert newMsg (Node left msg right)
  | newMsg `before` msg = Node (insert newMsg left) msg right
  | otherwise           = Node left msg (insert newMsg right)
  where before (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 < t2
        before  _ _                                    = False


------------------- Exercise 3 -------------------

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


------------------- Exercise 4 -------------------

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left msg right) = inOrder left ++ msg : inOrder right
inOrder  Leaf                 = []


------------------- Exercise 5 -------------------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr relevantMsg [] . inOrder . build
  where relevantMsg (LogMessage (Error i) _ msg) acc
          | i >= 50        = msg:acc
          | otherwise      = acc
        relevantMsg  _ acc = acc
