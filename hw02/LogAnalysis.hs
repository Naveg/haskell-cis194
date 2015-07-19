{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    "E" : lvl : t : msg -> LogMessage (Error (read lvl)) (read t) (unwords msg)
    "I" : t : msg -> LogMessage Info (read t) (unwords msg)
    "W" : t : msg -> LogMessage Warning (read t) (unwords msg)
    msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTime _) (Node left n@(LogMessage _ nodeTime _) right)
  | msgTime <= nodeTime = Node (insert msg left) n right
  | otherwise = Node left n (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

extractText :: LogMessage -> String
extractText (Unknown s) = s
extractText (LogMessage _ _ s) = s

badError :: LogMessage -> Bool
badError (LogMessage (Error lvl) _ _) = lvl >= 50
badError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map extractText (inOrder (build (filter badError msgs)))

