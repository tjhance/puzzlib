{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn, readFile)
import qualified Prelude as P

import Control.Monad

import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files

import Data.Char
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as L
import Data.Map

import GHC.Exts (sortWith)

main :: IO ()
main = do
    args <- getArgs
    if P.null args
        then usage
        else do
            sorted_words <- liftM BS.lines $ readFile (P.head args)
            let word_counts = getWordCounts sorted_words
            let sorted_word_counts = sortWith (negate . snd) word_counts
            sequence_ $ P.map (uncurry printCount) sorted_word_counts

printCount :: ByteString -> Int -> IO ()
printCount w c = P.putStr (show c) >> P.putStr " " >> BS.putStrLn w

usage :: IO ()
usage = do
    name <- getProgName
    P.putStrLn $ "Usage: " ++ name ++ " FILE"

getWordCounts :: [ByteString] -> [(ByteString, Int)]
getWordCounts ws = getWordCounts_ ws "" 0

getWordCounts_ :: [ByteString] -> ByteString -> Int -> [(ByteString, Int)]
getWordCounts_ [] last_w count = (last_w, count) : []
getWordCounts_ (w:ws) last_w count = if w == last_w
    then let count' = count+1 in seq count' $ getWordCounts_ ws last_w count'
    else if count /= 0
         then (last_w, count) : (getWordCounts_ ws w 1)
         else getWordCounts_ ws w 1
