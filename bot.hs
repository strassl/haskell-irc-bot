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
    write h "NICK" [nick]
    write h "USER" [nick, "0", "*", ":haskell bot"]
    write h "JOIN" [chan]
    listen h

write :: Handle -> Message -> IO ()
write h m = do
        hPrintf h "%s %s\r\n" action arg
        printf    "> %s %s\r\n" action arg
    where
        str = ':'

listen :: Handle -> IO ()
listen h = forever $ do
        l <- hGetLine h
        processLine h l



processLine :: Handle -> String -> IO ()
processLine h s = if isPing s then pong h (snd $ parseCommand s) else eval h s

eval :: Handle -> String -> IO ()
eval h s = putStrLn s

--isBotCommand :: String -> Bool
--isBotCommand s = 

--processBotCommand :: Handle -> String -> IO ()
--processBotCommand h s = 

isPing :: Message -> Bool
isPing m = (command m) == "PING"

pong :: Handle -> String -> IO ()
pong h server = write h "PONG" [server]
