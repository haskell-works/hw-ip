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

infixl 4 #<*>#

(#<*>#) :: AP.Parser Word8 -> AP.Parser Word8 -> AP.Parser Word8
(#<*>#) pa pb = paste <$> pa <*> pb
  where paste a b = a * 10 + b

octet :: AP.Parser Word8
octet = (ds 1 2 #<*>#  d 5  ) #<*># ds 0 5
  <|>   (ds 1 2 #<*># ds 0 4) #<*># ds 0 9
  <|>   ( d 1   #<*># ds 0 9) #<*># ds 0 9
  <|>    ds 1 9 #<*># ds 0 9
  <|>    ds 0 9

whitespace :: AP.Parser ()
whitespace = void $ many (AP.satisfy isSpace)

ipv4Address :: AP.Parser Word32
ipv4Address = fourOctetsToWord32
  <$> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*>  octet

ipv4NetMask :: AP.Parser Word8
ipv4NetMask =  d 3   #<*># ds 0 2
  <|>          d 2   #<*># ds 0 9
  <|>          d 1   #<*># ds 0 9
  <|>         ds 0 9

d :: Int -> AP.Parser Word8
d c      = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (== chr (c + 48))

ds :: Int -> Int -> AP.Parser Word8
ds c1 c2 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= chr (c1 + 48) && c <= chr (c2 + 48))

ipv4Block :: AP.Parser (Word32, Word8)
ipv4Block = do
  addr <- ipv4Address
  _    <- AP.char '/'
  mask <- ipv4NetMask
  return (addr, mask)

word32x4ToWords :: (Word32, Word32, Word32, Word32) -> [Word32]
word32x4ToWords (a, b, c, d) = [a, b, c, d]

bitPower :: Word8 -> Word64
bitPower m = fromIntegral (32 - m)

blockSize :: Word8 -> Int
blockSize m = 2 ^ bitPower m

bitPower128 :: Word8 -> Integer
bitPower128 m = fromIntegral (128 - m)

blockSize128 :: Word8 -> Integer
blockSize128 m = 2 ^ bitPower128 m
