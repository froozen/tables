module Hashing
    ( pair
    , HashPair(..)
    ) where

import Crypto.Hash (hash, Digest(..), MD5(..))
import Data.Bits (shift, shiftR)
import Data.Word (Word8)
import qualified Data.ByteString as L (pack, unpack)
import qualified Data.ByteString.Char8 as L8 (pack, unpack)

newtype HashPair = HashPair (String, String)

instance Show HashPair where
    show (HashPair (plain, hashed)) = hashed ++ " " ++ plain

pair :: String -> HashPair
pair s = HashPair (s, hashMD5 s)

hashMD5 :: String -> String
hashMD5 s = show (hash . L8.pack $ s :: Digest MD5)
