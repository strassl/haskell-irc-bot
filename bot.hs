import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Exception
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

data Bot = Bot { socket :: Handle 
               , active :: Bool }

type Net = ReaderT Bot IO

server = "irc.freenode.org"
port = 6667
chan = "#hbot-test"
nick = "hbotty"

-- bracket <before> <after> <in between>
main = bracket connect disconnect loop
    where
        loop st = runReaderT run st
        
-- Network stuff

connect :: IO Bot
connect = do 
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)

disconnect :: Bot -> IO ()
disconnect = hClose . socket

run :: Net ()
run = do
    write (cmdNick nick)
    write (cmdUser nick)
    write (cmdJoin chan)
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
        l <- liftIO $ hGetLine h
        liftIO $ putStrLn l
        processLine l

write :: Message -> Net ()
write m = do
    h <- asks socket
    liftIO $ hPrintf h str
    liftIO $ printf    str
    where
        str = compose m ++ "\r\n"

-- Text processing

processLine :: String -> Net ()
processLine s
    | isPing msg = write cmdPong
    | otherwise = eval msg
    where 
        server = head $ words $ trailing msg
        msg = parse s

eval :: Message -> Net ()
eval m
    | isBotCommand m = processBotCommand m
    | otherwise = return ()

-- Bot command processing

isBotCommand :: Message -> Bool
isBotCommand m = ("!" ++ nick) `isPrefixOf` s
    where
        s = trailing m

processBotCommand :: Message -> Net ()
processBotCommand m
    | act == "SAY" = write (cmdMsg chan (intercalate " " (args c)))
    | act == "QUIT" = write (cmdQuit "Going down!")
    | otherwise = write (Message "" "PRIVMSG" [chan] ("I'm sorry, " ++ prefix m ++ ". I'm afraid I can't do that."))
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

-- Common IRC commands

cmdNick n = Message "" "NICK" [n] ""

cmdUser n = Message "" "USER" [nick, "0", "*"] "bot"

cmdJoin c = Message "" "JOIN" [c] ""

cmdQuit m = Message "" "QUIT" [m] ""

cmdMsg r m = Message "" "PRIVMSG" [r] m

cmdPong = Message "" "PONG" [server] ""

isPing :: Message -> Bool
isPing m = (command m) == "PING"
