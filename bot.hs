{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import qualified Data.ByteString as B
import Data.Char
import Data.List
import qualified Database.PostgreSQL.Simple as PQ
import Network
import System.IO
import System.Exit
import Text.Printf

import IRC

data Command = Command { action :: String
                       , args   :: [String]
                       } deriving Show

data Bot = Bot { socket :: Handle 
               , operators :: [String]
               , db :: PQ.Connection
               }

type Net = ReaderT Bot IO

server = "irc.freenode.org"
port = 6667
chan = "#reddit"
nickname = "hbotty"
password = "password"

operatorsFile = "operators.txt"

-- bracket <before> <after> <in between>
main = bracket connect disconnect loop
    where
        loop st = runReaderT run st
        
-- Network stuff

connect :: IO Bot
connect = do 
    ops <- getOperators
    h <- connectTo server (PortNumber (fromIntegral port))
    c <- connectDB
    hSetBuffering h NoBuffering
    return (Bot h ops c)

disconnect :: Bot -> IO ()
disconnect = hClose . socket

run :: Net ()
run = do
    write (cmdPass password)
    write (cmdNick nickname)
    write (cmdUser nickname)
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

-- Database stuff

connectDB :: IO PQ.Connection
connectDB = PQ.connect info
    where
        info = PQ.defaultConnectInfo { PQ.connectUser = "bot", PQ.connectDatabase = "bot" }

-- Operator processing

getOperators :: IO [String]
getOperators = do
    f <- readFile operatorsFile
    return $ parseOperators f

parseOperators :: String -> [String]
parseOperators s = lines s

isOperator :: Message -> Net Bool
isOperator m = do
    ops <- asks operators
    return $ userInList m ops

userInList :: Message -> [String] -> Bool
userInList m l = elem (maybe "" nick u) l
    where
        u = parseUser (prefix m)

-- Text processing

processLine :: String -> Net ()
processLine s
    | isPing msg = write $ cmdPong (trailing msg)
    | otherwise = eval msg
    where 
        server = head $ words $ trailing msg
        msg = parse s

eval :: Message -> Net ()
eval m = do
    isOp <- isOperator m

    if isOp && isBotCommand m then processBotCommand m else return ()

-- Bot command processing

isBotCommand :: Message -> Bool
isBotCommand m = ("!" ++ nickname) `isPrefixOf` s
    where
        s = trailing m

processBotCommand :: Message -> Net ()
processBotCommand m
    | act == "SAY" = write (cmdMsg chan (intercalate " " (args c)))
    | act == "QUIT" = write (cmdQuit "Going down!")
    | otherwise = write (Message "" "PRIVMSG" [chan] ("I'm sorry, " ++ name ++ ". I'm afraid I can't do that."))
    where
        name = maybe "" nick (parseUser (prefix m))
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

cmdPass p = Message "" "PASS" [p] ""

cmdNick n = Message "" "NICK" [n] ""

cmdUser n = Message "" "USER" [n, "0", "*"] "bot"

cmdJoin c = Message "" "JOIN" [c] ""

cmdQuit m = Message "" "QUIT" [m] ""

cmdMsg r m = Message "" "PRIVMSG" [r] m

cmdPong s = Message "" "PONG" [s] ""

isPing :: Message -> Bool
isPing m = (command m) == "PING"
