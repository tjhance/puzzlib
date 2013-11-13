module Markov where

import Control.Monad

import Data.Char
import Data.List
import Data.Map (Map, findWithDefault, fromList)

import Util
import Frequency

data State = Start | End | Letter Char deriving (Eq, Show, Ord)

transitionTable :: [(Char, a)] -> [(String, a)] -> [(Char, a)] -> [((State, State), a)]
transitionTable initials bigrams finals =
    map (liftFst prepareInitial) initials ++
    map (liftFst prepareBigram) bigrams ++
    map (liftFst prepareFinal) finals

prepareInitial c = (Start, Letter c)
prepareFinal c = (Letter c, End)
prepareBigram b = (Letter (b !! 0), Letter (b !! 1))

groupAscListBy :: Eq b => (a -> b) -> [a] -> [[a]]
groupAscListBy _ [] = []
groupAscListBy f l@(a:as) = (takeWhile f' l) : (groupAscListBy f (dropWhile f' l))
                            where f' x = (f x == f a)

countsToProbabilities :: Integral b => [(a, b)] -> [(a, Float)]
countsToProbabilities l = map (liftSnd (\x -> fromIntegral x / total)) l
                          where total = fromIntegral . sum $ map snd l

conditionalProbabilityTable :: Integral a => [((State, State), a)] -> [((State, State), Float)]
conditionalProbabilityTable freqs =
    join $ map countsToProbabilities grouped_freqs where
    grouped_freqs = groupAscListBy (fst . fst) freqs

prepareString :: String -> [State]
prepareString s = [Start] ++ (map (Letter . toLower) (filter (\x -> isAlpha x && isAscii x) s)) ++ [End]

-- The main attraction
-- This returns the cumulative log probability of a simple markov chain
-- generating the given string. Obviously, this will decrease with the
-- word length, so results are only directly comparable between words
-- of the same length. I am still considering how to normalize to make
-- comparable results for differing lengths
englishMarkovScore :: String -> Float
englishMarkovScore = markovScore englishCLPTable

markovScore :: [((State, State), Float)] -> String -> Float
markovScore clpt = markovScore_ (fromList clpt) 0 . prepareString

markovScore_ :: Map (State, State) Float -> Float -> [State] -> Float
markovScore_ _ acc [] = acc
markovScore_ _ acc (s:[]) = acc
markovScore_ _ acc (Start:End:[]) = acc
markovScore_ clpm acc (s1:s2:ss) = seq acc' (markovScore_ clpm acc' (s2:ss))
    where acc' = acc + findWithDefault (-999.9) (s1, s2) clpm

englishCLPTable :: [((State, State), Float)]
englishCLPTable = map (liftSnd log) englishCPTable

englishCPTable :: [((State, State), Float)]
englishCPTable = conditionalProbabilityTable $ sort englishTTable
    where englishTTable = transitionTable englishInitials englishBigrams englishFinals
