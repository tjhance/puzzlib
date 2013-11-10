{-# LANGUAGE OverloadedStrings #-}
module Cryptogram where

import Prelude
import qualified Prelude as P

import Data.HashMap.Lazy
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
