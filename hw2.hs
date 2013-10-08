-- hw2.hs
-- Haskell UPenn Course
-- a log parser / analyzer

-- module
module LogAnalysis where

-- imports
import Log

-- strategy parse single message, into the types
-- parse a whole bunch of lines
-- put into some tree / order
-- answer of questions on it

-- parseMessage :: break into words, check if the words meet our criteria, force into their respective types
parseMessage :: String -> LogMessage
parseMessage str = case (head . words) str of
    "I" -> LogMessage Info    (grabPart 2 str) (keepString 3 str)
    "W" -> LogMessage Warning (grabPart 2 str) (keepString 3 str)
    "E" -> LogMessage (Error (grabPart 2 str)) (grabPart 3 str) (keepString 4 str)
    _   -> Unknown str

-- helper functions for the the various components
-- grabPart :: takes the string and grabs some component of it, reading it into whatever type it needs to be
grabPart :: Int -> String -> Int
grabPart n str = read (words str !! (n-1)) -- [1,2,3,4]
-- keepString :: takes the string, drops something off the front and returns the rest of the string
keepString :: Int -> String -> String
keepString n str = (unwords . drop (n-1) . words) str  -- doing n-1 to make the parseMessage in numeric order

-- parse :: parse a whole files into to a series of LogMessage's
parse :: String -> [LogMessage]
parse [] = []
parse str = map parseMessage (lines str)


-- create a binary search tree out of the LogMessages
-- using the ghetto form that is provided in Log.hs

-- singleTree :: defines what a single tree looks like
singleTree :: LogMessage -> MessageTree
singleTree lm = Node lm EmptyTree EmptyTree

--treeInsert :: insert something into the tree in ordered fashion based on TimeStamp
treeInsert :: LogMessage -> MessageTree -> MessageTree
treeInsert lm EmptyTree = singleTree lm
treeInsert (Unknown _) mt = mt
treeInsert lm@(LogMessage msgtype ts str) (Node omt@(LogMessage oldmsgtype oldts oldstr) left right) = case compare ts oldts of
    LT -> Node omt (treeInsert lm left) right
    GT -> Node omt left (treeInsert lm right)
    EQ -> Node omt left (treeInsert lm right) --not sure if this is needed, but I'll include it for now

-- build :: build up a MessageTree from a list of LogMessage's
build :: [LogMessage] -> MessageTree
build logs = foldr treeInsert EmptyTree logs --why foldr vs foldl...must lookup, again.

-- in-order traversal to sort out binary search tree
--inorderTrav :: goes left all the way down, then neutral, then right
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    EmptyTree -> []
    (Node v l r) -> inOrder l ++ [v] ++ inOrder r

-- whatWentWrong :: take unsorted list and return only those >50 severity, sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = ((map extractStr) . inOrder . build . (filter checkSev)) logs

quickTest logs = (inOrder . build . (filter checkSev)) logs

-- helpers for whatWentWrong 
-- checkSev :: check if I'm an Error type and > 50
checkSev :: LogMessage -> Bool
checkSev (LogMessage mt ts str) = case mt of
    (Error n) -> n > 50
    _ -> False

-- extractStr :: pull out the str from a LogMessage
extractStr :: LogMessage -> String
extractStr lm@(LogMessage msgtype ts str) = str
