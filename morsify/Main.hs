module Main where

import Morse
import System.Environment

data Options = Options { spaced :: Bool } deriving (Eq, Show, Ord)
defaultOptions = Options { spaced = False }

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    if null args
        then usage -- TODO: Change this to read from stdin
        else do
            contents <- readFile (args !! 0)
            let output = if spaced opts
                         then toSpacedMorse contents
                         else toMorse contents
            putStr output

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-s" -> parseOpts (as) (opts { spaced = True })
    x -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)

usage :: IO ()
usage = do
    name <- getProgName
    putStr . unlines $ [
        "Usage: " ++ name ++ " [OPTIONS] FILE",
        "Translate the specified file to Morse Code",
        "",
        "OPTIONS may include",
        "   -s      Include spaces after each morsified character"
        ]
