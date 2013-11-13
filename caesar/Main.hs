import Markov

import Control.Monad
import Data.Char

import System.Environment

import GHC.Exts (sortWith)

data Options = Options
  { o_all :: Bool
  , o_rated :: Bool
  } deriving (Eq, Show, Ord)

defaultOptions = Options
  { o_all = False
  , o_rated = True
  }

main :: IO ()
main = do
    args <- getArgs
    let (opts, args') = parseArgs defaultOptions args
    let shifts = caesarShifts . unwords $ args'
    if o_rated opts
        then goRated opts shifts
        else goUnrated opts shifts

goRated :: Options -> [String] -> IO ()
goRated opts shifts = do
    let ratedShifts = sortWith (negate . thd) $ zip3 [0..25] shifts (map rating shifts)
            where thd (x, y, z) = z
    if o_all opts
        then mapM_ printTriple ratedShifts
        else printTriple (head ratedShifts)

goUnrated :: Options -> [String] -> IO ()
goUnrated opts shifts = do
    let pairedShifts = zip [0..25] shifts
    mapM_ printPair pairedShifts

printPair (offset, shifted) = putStrLn $ (show offset) ++ "\t" ++ shifted
printTriple (offset, shifted, score) = putStrLn $ (show offset) ++ "\t" ++ shifted ++ "\t" ++ (show score)

parseArgs :: Options -> [String] -> (Options, [String])
parseArgs o [] = (o, [])
parseArgs o (a:as) = if head a == '-'
    then parseArgs (updateArgs a o) as
    else (o', a:as') where (o', as') = parseArgs o as

updateArgs :: String -> Options -> Options
updateArgs [] o = o
updateArgs (c:cs) o = case c of
    'a' -> updateArgs cs (o { o_all = True })
    'u' -> updateArgs cs (o { o_rated = False })
    'o' -> updateArgs cs (o { o_rated = False })
    'r' -> updateArgs cs (o { o_rated = True })
    _ -> updateArgs cs o

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " STRING"

shift n c = if c >= 'A' && c <= 'Z' then chr ((ord c - ord 'A' + n) `mod` 26 + ord 'A') else c

caesarShifts :: String -> [String]
caesarShifts s = map (\n -> map (shift n) s') [0..25] where s' = map toUpper s

rating :: String -> Float
rating = sum . map englishMarkovScore . words
