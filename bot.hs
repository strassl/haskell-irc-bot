import Control.Monad
import Data.Char
import Data.List
import Network
import System.IO
import System.Exit
import Text.Printf

import IRC

data Command = Command { action :: String
                       , args   :: [String]
                       } deriving Show

server = "irc.freenode.org"
port = 6667
chan = "#hbot-test"
nick = "hbotty"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h (Message "" "NICK" [nick] "")
    write h (Message "" "USER" [nick, "0", "*"] "haskell bot")
    write h (Message "" "JOIN" [chan] "")
    listen h

write :: Handle -> Message -> IO ()
write h m = do
        hPrintf h str
        printf    str
    where
        str = compose m ++ "\r\n"

listen :: Handle -> IO ()
listen h = forever $ do
        l <- hGetLine h
        processLine h l

processLine :: Handle -> String -> IO ()
processLine h s
    | isPing msg = write h pong
    | otherwise = eval h msg
    where 
        pong = Message "" "PONG" [server] ""
        server = head $ words $ trailing msg
        msg = parse s

eval :: Handle -> Message -> IO ()
eval h m
    | isBotCommand m = processBotCommand h m
    | otherwise = putStrLn (compose m)

isBotCommand :: Message -> Bool
isBotCommand m = ("!" ++ nick) `isPrefixOf` s
    where
        s = trailing m

processBotCommand :: Handle -> Message -> IO ()
processBotCommand h m
    | act == "SAY" = write h (Message "" "PRIVMSG" [chan] (intercalate " " (args c)))
    | otherwise = write h (Message "" "PRIVMSG" [chan] ("I'm sorry, " ++ prefix m ++ ". I'm afraid I can't do that."))
    where
        act = action c
        c = parseCommand m

parseCommand :: Message -> Command
parseCommand m = parseArgs arguments
    where
        arguments = drop 1 (words str)
        str = trailing m

        parseArgs [] = Command "" []
        parseArgs [x] = Command x []
        parseArgs (x:xs) = Command x xs

isPing :: Message -> Bool
isPing m = (command m) == "PING"
