{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn, readFile)
import qualified Prelude as P

import System.Environment
import System.IO

import Data.Char
import Data.List
import Data.Ord

import GHC.Exts

data Options = Options { dictionary :: String
                       , subwords :: Bool
                       , minLength :: Int
                       }
defaultOptions = Options { dictionary = "/usr/share/dict/words"
                         , subwords = False
                         , minLength = 0
                         }

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    if P.null args
        then usage
        else do
            words <- fmap lines . readFile $ dictionary opts
            putStr . unlines $ sortWith (negate . length) . filter (matches (subwords opts) (makePattern $ head args)) . filter (\x -> length x >= minLength opts) $ words

data Symbol = Exact Char | Wild deriving (Eq, Show, Ord)

matches :: Bool -> [Symbol] -> String -> Bool
matches False pattern = matchesFull pattern . sort
matches True pattern = matchesPartial pattern . sort

matchesFull pattern s = matchesFull' pattern s [] where
    matchesFull' [] [] [] = True
    matchesFull' [] _  _  = False
    matchesFull' p@(x:xs) [] r = case x of Wild -> length p == length r
                                           Exact a -> False
    matchesFull' p@(x:xs) s@(c:cs) r = case x of Wild -> length p == length s + length r
                                                 Exact a -> case compare a c of EQ -> matchesFull' xs cs r
                                                                                LT -> False
                                                                                GT -> matchesFull' p cs (c:r)

matchesPartial pattern s = matchesPartial' pattern s [] where
    matchesPartial' [] [] [] = True
    matchesPartial' [] _  _  = False
    matchesPartial' p@(x:xs) [] r = case x of Wild -> length p >= length r
                                              Exact a -> matchesPartial' xs [] r
    matchesPartial' p@(x:xs) s@(c:cs) r = case x of Wild -> length p >= length s + length r
                                                    Exact a -> case compare a c of EQ -> matchesPartial' xs cs r
                                                                                   LT -> matchesPartial' xs s r
                                                                                   GT -> matchesPartial' p cs (c:r)

makePattern :: String -> [Symbol]
makePattern = sort . makePattern' where
    makePattern' [] = []
    makePattern' ('.':cs) = Wild : makePattern cs
    makePattern' ('?':cs) = Wild : makePattern cs
    makePattern' (c:cs) = Exact c : makePattern cs

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-d"        -> parseOpts (P.tail as) (opts { dictionary = P.head as })
    "-w"        -> parseOpts as (opts { subwords = True })
    "-m"        -> parseOpts (P.tail as) (opts { minLength = read $ P.head as })
    x           -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)

usage :: IO ()
usage = do
    name <- getProgName
    P.putStrLn $ "Usage: " ++ name ++ " [OPTIONS] PATTERN"
