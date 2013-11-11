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
            words <- recurseGetWords (P.head args)
            sequence_ $ P.map putStrLn words
            -- printWordFrequencies words

-- printWordFrequencies :: [ByteString] -> IO ()
-- printWordFrequencies ws = do
--     let sorted_ws = L.sort ws
--     P.print sorted_ws
--     -- let freq_table = buildWordFrequencies ws
--     -- P.print freq_table
--     -- let freq_list = sortWith (negate . snd) $ toList freq_table
--     -- sequence_ $ P.map P.print freq_list

buildWordFrequencies :: [ByteString] -> Map ByteString Int
buildWordFrequencies ws = fromListWith (+) (P.map (\x -> (x, 1)) ws)

usage :: IO ()
usage = do
    name <- getProgName
    P.putStrLn $ "Usage: " ++ name ++ " SOURCEDIR"

cleanWord :: ByteString -> ByteString
cleanWord = BS.map toLower . BS.filter (\x -> isAlphaNum x && isAscii x)

recurseGetWords :: FilePath -> IO [ByteString]
recurseGetWords path = do
    stat <- getFileStatus path
    if isDirectory stat
        then do
            files' <- getDirectoryContents path
            let files = P.filter (\x -> x /= ".." && x /= ".") files'
            liftM P.concat (sequence $ P.map recurseGetWords (P.map (path </>) files))
        else if takeExtension path == ".txt"
            then do
                contents <- readFile path
                return $! P.filter (not . BS.null) . P.map cleanWord $ BS.words contents
            else return []
