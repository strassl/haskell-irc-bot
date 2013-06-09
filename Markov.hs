{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Markov (parse, createText, produce, insertChain) where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PQ
import Database.PostgreSQL.Simple.FromRow
import System.IO.Unsafe (unsafeInterleaveIO)

type Chain = [Edge]
type Edge = (Word, Word)
type Word = T.Text

-- Pure parsing

parse :: T.Text -> Chain
parse s = chain list
    where
        list = tokenize s

chain :: [Word] -> Chain
chain [word, last] = [(word, last)]
chain (x:xs) = (x, head xs) : (chain xs)

tokenize :: T.Text -> [Word]
tokenize = (T.split isSpaceNoCrLf) . (T.replace "\n" " \n ")

isSpaceNoCrLf :: Char -> Bool
isSpaceNoCrLf c = c `elem`  " \t\f\v\xa0"

-- Impure database stuff

data Row = Row { id :: Integer
               , word :: Word
               , nextword :: Word
               } deriving (Show)

instance PQ.FromRow Row where
    fromRow = Row <$> field <*> field <*> field 

createText c w i = do
    l <- produce c w
    return $ T.intercalate " " (take i l)

produce :: PQ.Connection -> Word -> IO [Word]
produce c w = unsafeInterleaveIO $ do
    nw <- nextWord c w
    l <- produce c nw
    return (w:l)

nextWord :: PQ.Connection -> Word -> IO Word
nextWord c w = do
    xs :: [Row] <- PQ.query c "SELECT * FROM markov WHERE word=? ORDER BY RANDOM() LIMIT 1" [w]
    fallback :: [Row] <- PQ.query c "SELECT * FROM markov ORDER BY RANDOM() LIMIT 1" ()
    if (length xs) > 0
        then return $ nextword $ head xs
        else return $ nextword $ head fallback

insertChain :: PQ.Connection -> Chain -> IO ()
insertChain c chain = sequence_ $ map (insertEdge c) chain

insertEdge :: PQ.Connection -> Edge -> IO ()
insertEdge c e = do 
    PQ.execute c "INSERT INTO markov (word, nextword) VALUES (?,?)" (fst e, snd e)
    return ()

getEdges :: PQ.Connection -> Word -> IO [Row]
getEdges c w = PQ.query c "SELECT * FROM markov WHERE word = ?" [w]

getEdge :: PQ.Connection -> Edge -> IO [Row]
getEdge c e = PQ.query c "SELECT * FROM markov WHERE word = ? AND nextword = ?" (fst e, snd e)

getStoredCount :: PQ.Connection -> Edge -> IO Int
getStoredCount c e = do
    rows <- getEdge c e
    return $ length rows
