{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn, readFile)
import qualified Prelude as P

import System.Environment
import System.IO

import Control.Monad

import Data.Char
import Data.List

import Text.Printf

import GHC.Exts

data Cost = Cost Double | Infinity deriving (Eq, Show, Ord)
instance Num Cost where
    Cost x + Cost y = Cost (x + y)
    Infinity + _ = Infinity
    _ + Infinity = Infinity

    Cost x * Cost y = Cost (x * y)
    Infinity * _ = Infinity
    _ * Infinity = Infinity

    negate (Cost x) = Cost (-x)
    negate Infinity = Infinity

    abs (Cost x) = Cost (abs x)
    abs Infinity = Infinity

    signum (Cost x) = Cost (signum x)
    signum Infinity = Infinity

    fromInteger = Cost . fromInteger

instance Fractional Cost where
    Cost x / Cost y = Cost (x / y)
    Infinity / _ = Infinity
    _ / Infinity = 0
    fromRational = Cost . fromRational

type Metric = Char -> Char -> Cost

-- Metric choices
exact :: Metric
exact x y = if x == y then 0 else Infinity

hamming :: Metric
hamming x y = if x == y then 0 else 1

line :: Metric
line x y = if isAlpha x && isAlpha y then Cost . fromIntegral $ line' x y else exact x y
    where line' x y = abs (ord (toLower x) - ord (toLower y))

arc :: Metric
arc x y = if isAlpha x && isAlpha y then Cost . fromIntegral $ 26 + arc' x y else exact x y
    where arc' x y = f (abs $ (ord . toLower $ x) - (ord . toLower $ y))
          f x = min (x `mod` 26) (26 - (x `mod` 26))

data Options = Options { dictionary :: String
                       , metric :: Metric 
                       , cnt :: Int
                       , quiet :: Bool
                       , max_dist :: Cost
                       }
defaultOptions = Options { dictionary = "/usr/share/dict/words"
                         , metric = hamming
                         , cnt = 20
                         , quiet = False
                         , max_dist = 0
                         }

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    if P.null args
        then usage
        else do
            words <- fmap lines . readFile $ dictionary opts
            let pattern = makePattern (head args)
            printResults pattern opts $ matches (metric opts) pattern words (max_dist opts)

matches metric pattern words radius = takeWhile ((<= radius) . grade metric pattern) . sortWith (grade metric pattern) . filter ((/= Infinity) . grade metric pattern) $ words

-- Assumes that all strings have length equal to the length of the pattern
printResults pattern opts words = do
    putStr . unlines . take (cnt opts) $ words
    if quiet opts then return () else do
        printf "------------------\n"
        printf "%d total matches\n\n" (length words)
        printf "Positional entropy\n"
        printf "------------------\n"
        forM_ gradedPositions (\(e,p) -> printf "%d\t%f bits\n" p e)
    where gradedPositions = reverse . sort . filter (\(e,p) -> e > 0) . map (\p -> (positionEntropy (p-1) words, p)) $ [1..length pattern]


positionEntropy k = entropy . map (fromIntegral . length) . groupWith (!! k)

entropy :: [Double] -> Double
entropy xs = sum . map (\x -> -x * log x / log 2) . map (/ sum xs) . filter (/= 0) $ xs

makePattern :: String -> [Maybe Char]
makePattern [] = []
makePattern ('.':cs) = Nothing : makePattern cs
makePattern ('?':cs) = Nothing : makePattern cs
makePattern (c:cs) = Just c : makePattern cs

grade :: Metric -> [Maybe Char] -> String -> Cost
grade m [] [] = Cost 0
grade m [] _ = Infinity
grade m _ [] = Infinity
grade m (x:xs) (y:ys) = m' x y + grade m xs ys
    where m' (Just x) y = m x y
          m' Nothing y = 0

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-d"        -> parseOpts (P.tail as) (opts { dictionary = P.head as })
    "--arc"     -> parseOpts as (opts { metric = arc })
    "--line"    -> parseOpts as (opts { metric = line })
    "-h"        -> parseOpts as (opts { metric = hamming })
    "--hamming" -> parseOpts as (opts { metric = hamming })
    "-n"        -> parseOpts (P.tail as) (opts { cnt = read $ P.head as })
    "-q"        -> parseOpts as (opts { quiet = True })
    "-r"        -> parseOpts (P.tail as) (opts { max_dist = Cost . read $ P.head as })
    x           -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)

usage :: IO ()
usage = do
    name <- getProgName
    P.putStr . unlines $ [
        "Usage: " ++ name ++ " [OPTIONS] PATTERN",
        "Search for words matching the given pattern. The pattern is a simple string",
        "that may include wildcards, which are denoted with eitehr a period (.)",
        "or a question mark (?). You can also search for partial matches with",
        "various metrics. By default, the Hamming metric is used with max distance 0.",
        "Example: " ++ name ++ " ...er.l",
        "",
        "OPTIONS may include",
        "   -d dictionary       Set the dictionary to use (default: /usr/share/dict/words)",
        "   --arc               Use a variant of the metric treating the alphabet as a circle",
        "   --line              Use a variant of the metric treating the alphabet as a line",
        "   -h/--hamming        Use the Hamming metric",
        "   -n number           Set how many of the top results to print (default: 20)",
        "   -q                  Do not print the entropy information",
        "   -r                  Set the maximum distance from the pattern (default: 0)"
        ]
