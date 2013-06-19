{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Data
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PQ
import GHC.Generics
import Network
import System.Exit
import System.IO
import System.Random
import Text.Printf

import IRC
import Markov (createText)

data Command = Command { action :: String
                       , args   :: [String]
                       } deriving Show

data Config = Config { server   :: String
                     , port     :: Int
                     , chan     :: String
                     , nickname :: String
                     , password :: String
                     } deriving (Show, Generic)

instance FromJSON Config

data Bot = Bot { socket :: Handle 
               , config :: Config
               , operators :: [String]
               , connection :: PQ.Connection
               }

type Net = ReaderT Bot IO

operatorsPath = "operators.txt"
configPath = "config.json"

-- bracket <before> <after> <in between>
main = bracket connect disconnect loop
    where
        loop st = runReaderT run st
        
-- Network stuff

connect :: IO Bot
connect = do 
    confFile <- BL.readFile configPath
    let conf = fromJust $ (decode confFile :: Maybe Config)
    ops <- getOperators
    h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
    c <- connectDB
    hSetBuffering h NoBuffering
    return (Bot h conf ops c)

disconnect :: Bot -> IO ()
disconnect = hClose . socket

run :: Net ()
run = do
    conf <- asks config
    write (cmdPass (password conf))
    write (cmdNick (nickname conf))
    write (cmdUser (nickname conf))
    write (cmdJoin (chan conf))
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

-- Logging

logFilter :: [String]
logFilter = [ "Excess Flood", "Client Quit", "Remote host closed", "*.net *.split", "Read error"
            , "Ping timeout", "Quit:" ]

shouldLog :: Config -> Message -> Bool
shouldLog conf m = ((command m) == "PRIVMSG") && 
                   (isUserPrefix $ prefix m) &&
                   (not $ isBotCommand conf m) &&
                   (not $ isEmpty (trailing m)) &&
                   (not $ or (map lFilter logFilter))
    where
        lFilter = flip isInfixOf (trailing m)
        isEmpty s = (null s) || (and $ map isSpace s)

logLine :: Message -> Net ()
logLine m = do
    conf <- asks config

    if shouldLog conf m
        then liftIO $ appendFile ((chan conf) ++ "_log.txt") ((trailing m) ++ "\n")
        else liftIO $ return ()

-- Database stuff

connectDB :: IO PQ.Connection
connectDB = PQ.connect info
    where
        info = PQ.defaultConnectInfo { PQ.connectUser = "bot", PQ.connectDatabase = "bot" }

-- Operator processing

getOperators :: IO [String]
getOperators = do
    f <- readFile operatorsPath
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
    conf <- asks config
    logLine m

    if isOp && isBotCommand conf m then processBotCommand conf m else return ()

-- Bot command processing

isBotCommand :: Config -> Message -> Bool
isBotCommand conf m = ("!" ++ (nickname conf)) `isPrefixOf` s
    where
        s = trailing m

processBotCommand :: Config -> Message -> Net ()
processBotCommand conf m
    | act == "SAY" = write (cmdMsg (chan conf) (intercalate " " (args c)))
    | act == "QUIT" = write (cmdQuit "Going down!")
    | act == "CHAIN" = processMarkov c
    | otherwise = write (Message "" "PRIVMSG" [(chan conf)] ("I'm sorry, " ++ name ++ ". I'm afraid I can't do that."))
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

processMarkov :: Command -> Net ()
processMarkov c = do
    db <- asks connection 
    conf <- asks config
    txt <- liftIO $ createText db start num

    write $ cmdMsg (chan conf) (T.unpack (T.replace "\n" "" txt))

    where
        a = args c
        start = T.pack (a !! 0)
        num = read (a !! 1) :: Int

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
