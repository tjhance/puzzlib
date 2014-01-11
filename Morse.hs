module Morse where

import Data.Char
import Data.Map (Map)
import Data.Tuple
import qualified Data.Map as Map

codes = [ ('A', ".-")
        , ('B', "-...")
        , ('C', "-.-.")
        , ('D', "-..")
        , ('E', ".")
        , ('F', "..-.")
        , ('G', "--.")
        , ('H', "....")
        , ('I', "..")
        , ('J', ".---")
        , ('K', "-.-")
        , ('L', ".-..")
        , ('M', "--")
        , ('N', "-.")
        , ('O', "---")
        , ('P', ".--.")
        , ('Q', "--.-")
        , ('R', ".-.")
        , ('S', "...")
        , ('T', "-")
        , ('U', "..-")
        , ('V', "...-")
        , ('W', ".--")
        , ('X', "-..-")
        , ('Y', "-.--")
        , ('Z', "--..")
        , ('1', ".----")
        , ('2', "..---")
        , ('3', "...--")
        , ('4', "....-")
        , ('5', ".....")
        , ('6', "-....")
        , ('7', "--...")
        , ('8', "---..")
        , ('9', "----.")
        , ('0', "-----")
        ]
codeMap = Map.fromList codes
letterMap = Map.fromList (map swap codes)

toMorse :: String -> String
toMorse "" = ""
toMorse (c:cs) = Map.findWithDefault (c:"") (toUpper c) codeMap ++ toMorse cs

-- TODO: Avoid extraneous space at the end of words
toSpacedMorse :: String -> String
toSpacedMorse "" = ""
toSpacedMorse (c:"") = Map.findWithDefault (c:"") (toUpper c) codeMap
toSpacedMorse (c:cs) = piece ++ toSpacedMorse cs
    where piece = case Map.lookup (toUpper c) codeMap of
            Nothing -> c:""
            Just s -> s ++ " "
