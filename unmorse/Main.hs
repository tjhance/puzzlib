module Main where

import Morse
import System.Environment
import System.IO

data Options = Options { dictionary :: String } deriving (Eq, Show, Ord)
defaultOptions = Options { dictionary = "/usr/share/dict/words" }

matches :: String -> String -> Bool
matches "" "" = True
matches "" _ = False
matches _ "" = False
matches (a:as) (b:bs) = (a == b || a == '?' || b == '?') && matches as bs

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    if null args
        then usage
        else do
            words <- fmap lines . readFile $ dictionary opts
            let pattern = head args
            let matched = filter (matches pattern . toMorse) words
            sequence_ (map ((>> hFlush stdout) . putStrLn) matched)

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-d" -> parseOpts (tail as) (opts { dictionary = head as })
    x    -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " [OPTIONS] PATTERN"
