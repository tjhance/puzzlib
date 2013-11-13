import Markov

import Control.Monad
import Data.Char

import System.Environment

import GHC.Exts (sortWith)

main :: IO ()
main = do
    args <- getArgs
    let shifts = caesarShifts . unwords $ args
    let ratedShifts = zip3 [0..25] shifts (map rating shifts)
    let sortedShifts = sortWith (negate . thd) ratedShifts
            where thd (x,y,z) = z
    mapM_ printLine sortedShifts
            where printLine (offset,shifted,score) = putStrLn $ (show offset) ++ "\t" ++ shifted ++ "\t" ++ (show score)

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " STRING"

shift n c = if c >= 'A' && c <= 'Z' then chr ((ord c - ord 'A' + n) `mod` 26 + ord 'A') else c

caesarShifts :: String -> [String]
caesarShifts s = map (\n -> map (shift n) s') [0..25] where s' = map toUpper s

rating :: String -> Float
rating = sum . map englishMarkovScore . words
