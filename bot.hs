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
    write h (cmdNick nick)
    write h (cmdUser nick)
    write h (cmdJoin chan)
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
    | isPing msg = write h cmdPong
    | otherwise = eval h msg
    where 
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
    | act == "SAY" = write h (cmdMsg chan (intercalate " " (args c)))
    | act == "QUIT" = write h (cmdQuit "Going down!")
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

-- Common IRC commands

cmdNick n = Message "" "NICK" [n] ""

cmdUser n = Message "" "USER" [nick, "0", "*"] "bot"

cmdJoin c = Message "" "JOIN" [c] ""

cmdQuit m = Message "" "QUIT" [m] ""

cmdMsg r m = Message "" "PRIVMSG" [r] m

cmdPong = Message "" "PONG" [server] ""
