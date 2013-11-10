module Cryptogram where

import Data.Map

canonicalize :: String -> [Int]
canonicalize s = canonicalize_ s empty 0

canonicalize_ :: String -> Map Char Int -> Int -> [Int]
canonicalize_ [] _ _ = []
canonicalize_ (c:cs) m next = if c `member` m
    then (m ! c) : (canonicalize_ cs m next)
    else next : (canonicalize_ cs (insert c next m) (next+1))
