import Control.Monad
import Data.Char
import Data.List
import Network
import System.IO
import System.Exit
import Text.Printf

import IRC

server = "irc.freenode.org"
port = 6667
chan = "#hbot-test"
nick = "haskell-botty"

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
eval h m = putStrLn (compose m)

--isBotCommand :: String -> Bool
--isBotCommand s = 

--processBotCommand :: Handle -> String -> IO ()
--processBotCommand h s = 

isPing :: Message -> Bool
isPing m = (command m) == "PING"
