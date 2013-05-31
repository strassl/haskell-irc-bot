{-# LANGUAGE OverloadedStrings #-}

module Markov (parse) where

import Data.Char
import qualified Data.Text as T

parse :: T.Text -> [(T.Text, T.Text)]
parse s = chain list
    where
        list = tokenize s

chain :: [T.Text] -> [(T.Text, T.Text)]
chain [word, last] = [(word, last)]
chain (x:xs) = (x, head xs) : (chain xs)

tokenize :: T.Text -> [T.Text]
tokenize = (T.split isSpaceNoCrLf) . (T.replace "\n" " \n ")

isSpaceNoCrLf :: Char -> Bool
isSpaceNoCrLf c = c `elem`  " \t\f\v\xa0"
