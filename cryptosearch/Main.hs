{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn, readFile)
import qualified Prelude as P

import Cryptogram

import System.Environment
import System.IO hiding (putStrLn, readFile)

import Data.ByteString.Char8 hiding (filter)
import qualified Data.ByteString.Char8 as BS

data Options = Options { dictionary :: String } deriving (Eq, Show, Ord)
defaultOptions = Options { dictionary = "/usr/share/dict/words" }

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    if P.null args
        then usage
        else do
            words <- fmap BS.lines . BS.readFile $ dictionary opts
            let pattern = canonicalize (pack $ P.head args)
            let matches = filter ((== pattern) . canonicalize) words
            sequence_ (P.map putStrLn matches)

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-d" -> parseOpts (P.tail as) (opts { dictionary = P.head as })
    x    -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)

usage :: IO ()
usage = do
    name <- getProgName
    P.putStrLn $ "Usage: " ++ name ++ " [OPTIONS] PATTERN"
