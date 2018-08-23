{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Ip.Internal where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Attoparsec.Text as AP

fourOctetsToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fourOctetsToWord32 a b c d =
  (fromIntegral a .<. 24) .|.
  (fromIntegral b .<. 16) .|.
  (fromIntegral c .<.  8) .|.
   fromIntegral d
{-# INLINE fourOctetsToWord32 #-}

infixl 2 #<*>#

(#<*>#) :: AP.Parser Word8 -> AP.Parser Word8 -> AP.Parser Word8
(#<*>#) pa pb = paste <$> pa <*> pb
  where paste a b = a * 10 + b

octet :: AP.Parser Word8
octet = ((d12 #<*># d5 ) #<*># d05)
  <|>   ((d12 #<*># d04) #<*># d09)
  <|>   ( d19 #<*># d09)
  <|>   d09
  where d5  = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (== '5')
        d04 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= '0' && c <= '4')
        d05 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= '0' && c <= '5')
        d09 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= '0' && c <= '9')
        d19 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= '1' && c <= '9')
        d12 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= '1' && c <= '2')

ipv4Address :: AP.Parser Word32
ipv4Address = fourOctetsToWord32
  <$> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*>  octet

whitespace :: AP.Parser ()
whitespace = void $ many (AP.satisfy isSpace)
