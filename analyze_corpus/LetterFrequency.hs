import Prelude
import qualified Prelude as P

import Control.Monad

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as L
import Data.Map

import System.Environment
import System.IO

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
    P.putStrLn "Letter frequencies"
    let letterFreqTable = toList $ letterFreq counts
    let sortedLetters = sortWith (negate . snd) letterFreqTable
    sequence_ (P.map print sortedLetters)
    P.putStr "Letter entropy: "
    P.print $ entropy (P.map snd sortedLetters)
    hFlush stdout

    P.putStrLn "Initial letter frequencies"
    let initialLetters = sortWith (negate . snd) . toList $ freq (return . BS.head) counts
    sequence_ (P.map print initialLetters)
    hFlush stdout

    P.putStrLn "Final letter frequencies"
    let finalLetters = sortWith (negate . snd) . toList $ freq (return . BS.last) counts
    sequence_ (P.map print finalLetters)
    hFlush stdout

    P.putStrLn "Bigram frequencies"
    let bigrams = sortWith (negate . snd) . toList $ freq (kgrams 2) counts
    sequence_ (P.map print bigrams)

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

liftFst f (x,y) = (f x, y)
liftSnd f (x,y) = (x, f y)

freq :: Ord a => (ByteString -> [a]) -> [(ByteString, Int)] -> Map a Int
freq f wordcounts = L.foldl' addData Data.Map.empty (P.map (liftFst f) wordcounts)

kgrams :: Int -> ByteString -> [ByteString]
kgrams k w = if k > BS.length w then [] else (BS.take k w) : (kgrams k (BS.drop 1 w))

freqToProb :: [(a, Int)] -> [(a, Float)]
freqToProb l = let total = fromIntegral $ L.sum (L.map snd l) in
    L.map (liftSnd ((/ total) . fromIntegral)) l

entropy :: [Int] -> Float
entropy freqs = let total = fromIntegral $ L.sum freqs
                    probs = L.map ((/ total) . fromIntegral) freqs in L.sum $ L.map (\p -> -p * log p) probs

