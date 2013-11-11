import Prelude
import qualified Prelude as P

import Control.Monad

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as L
import Data.Map

import System.Environment

import GHC.Exts (sortWith)

main :: IO ()
main = do
    args <- getArgs
    if P.null args
        then usage
        else go (P.head args)

go :: FilePath -> IO ()
go path = do
    counts <- readCounts path
    let letterFreqTable = toList $ letterFreq counts
    let sortedLetters = sortWith (negate . snd) letterFreqTable
    sequence_ (P.map print sortedLetters)

usage :: IO ()
usage = do
    name <- getProgName
    P.putStrLn $ "Usage: " ++ name ++ " COUNTFILE"

readCounts :: FilePath -> IO [(ByteString, Int)]
readCounts path = do
    contents <- liftM BS.lines $ BS.readFile path
    return (P.map parseCount contents)

parseCount :: ByteString -> (ByteString, Int)
parseCount l = let (l1, l2) = BS.break (== ' ') l in (BS.tail l2, read $ BS.unpack l1)

letterFreq :: [(ByteString, Int)] -> Map Char Int
letterFreq = freq unpack

addData :: Ord a => Map a Int -> ([a], Int) -> Map a Int
addData m (dat, count) = L.foldl' (flip (\x -> insertWith (+) x count)) m dat

liftFst :: (a -> b) -> (a,c) -> (b,c)
liftFst f (x,y) = (f x, y)

freq :: Ord a => (ByteString -> [a]) -> [(ByteString, Int)] -> Map a Int
freq f wordcounts = L.foldl' addData Data.Map.empty (P.map (liftFst f) wordcounts)
