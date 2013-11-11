{-# LANGUAGE OverloadedStrings #-}
module Cryptogram where

import Prelude
import qualified Prelude as P

import Data.HashMap.Lazy
import qualified Data.HashSet as HS

import Data.Word (Word8)
import Data.ByteString hiding (empty)
import qualified Data.ByteString as BS

canonicalize :: ByteString -> [Int]
canonicalize s = canonicalize_ s empty 0

canonicalize_ :: ByteString -> HashMap Word8 Int -> Int -> [Int]
canonicalize_ "" _ _ = []
canonicalize_ s m next = let c = BS.head s in
    if c `member` m
    then (m ! c) : (canonicalize_ (BS.tail s) m next)
    else next : (canonicalize_ (BS.tail s) (insert c next m) (next+1))

-- Lower case letters are arbitrary while all others are treated as fixed
data Symbol = Option Int | Fixed Word8 deriving (Eq, Show, Ord)

isUpperLetter :: Word8 -> Bool
isUpperLetter x = x >= 65 && x <= 90

isLowerLetter :: Word8 -> Bool
isLowerLetter x = x >= 97 && x <= 122

toLower :: Word8 -> Word8
toLower x = if isUpperLetter x then x + 32 else x

getPattern :: ByteString -> [Symbol]
getPattern s = getPattern_ s empty 0

getPattern_ :: ByteString -> HashMap Word8 Int -> Int -> [Symbol]
getPattern_ "" _ _ = []
getPattern_ s m next = let c = BS.head s in
    if isLowerLetter c
    then if c `member` m
         then (Option (m ! c)) : (getPattern_ (BS.tail s) m next)
         else (Option next) : (getPattern_ (BS.tail s) (insert c next m) (next+1))
    else (Fixed $ toLower c) : getPattern_ (BS.tail s) m next

fixedSet :: [Symbol] -> HS.HashSet Word8
fixedSet [] = HS.empty
fixedSet ((Option _):ss) = fixedSet ss
fixedSet ((Fixed c):ss) = HS.insert c (fixedSet ss)

matchesPattern :: [Symbol] -> ByteString -> Bool
matchesPattern pattern = matchesPattern_ pattern (fixedSet pattern) empty 0

matchesPattern' :: [Symbol] -> HS.HashSet Word8 -> ByteString -> Bool
matchesPattern' pattern fixed = matchesPattern_ pattern fixed empty 0

matchesPattern_ :: [Symbol] -> HS.HashSet Word8 -> HashMap Word8 Int -> Int -> ByteString -> Bool
matchesPattern_ [] _ _ _ str = BS.null str
matchesPattern_ ((Option ind):ss) fixed m next str = not (BS.null str) && let c = BS.head str in
    if c `member` m
    then (m ! c) == ind && not (c `HS.member` fixed) && matchesPattern_ ss fixed m next (BS.tail str)
    else next == ind && not (c `HS.member` fixed) && matchesPattern_ ss fixed (insert c next m) (next+1) (BS.tail str)
matchesPattern_ ((Fixed s):ss) fixed m next str = not (BS.null str) && BS.head str == s && matchesPattern_ ss fixed m next (BS.tail str)
