module IRC (Message (..), User (..), parse, compose, parseUser) where

import Data.Char
import Data.List
import Data.Text as T (Text, pack, unpack,  split)

data Message = Message { prefix :: String
                       , command :: String
                       , params :: [String]
                       , trailing :: String
                       } deriving Show

data User = User { nick :: String
                 , user :: String
                 , host :: String
                 } deriving Show

parse :: String -> Message
parse s = Message prefix command params trailing
    where
        prefix = drop 1 $ if hasPrefix (head split) then head split else ""
        command = bare !! 0
        params = takeWhile (not . hasPrefix) (drop 1 bare)
        trailing = drop 1 $ intercalate " " $ dropWhile (not . hasPrefix) (drop 1 bare)
        bare = if hasPrefix (head split) then drop 1 split else split
        split = words s

compose :: Message -> String
compose m = intercalate " " $ filter (/="") [preStr, command m, paraStr, trailStr]
    where
        preStr = if (length $ prefix m) > 0 then ':' : (prefix m) else ""
        paraStr = intercalate " " (params m)
        trailStr = if (length $ trailing m) > 0 then ':' : (trailing m) else ""

hasPrefix :: String -> Bool
hasPrefix s = head s == ':'

isUserPrefix :: String -> Bool
isUserPrefix = elem '!'

parseUser :: String -> Maybe User
parseUser s = if isUserPrefix s then Just (User n u h) else Nothing
    where
        n = head l
        u = l !! 1
        h = last l
        l = map unpack $ split (flip elem "!@") (pack s)
