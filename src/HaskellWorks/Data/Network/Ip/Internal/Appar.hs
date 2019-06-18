module HaskellWorks.Data.Network.Ip.Internal.Appar
  ( fourOctetsToWord32
  , (#<*>#)
  , octet
  , whitespace
  , ipv4Address
  , ipv4NetMask
  , digit
  , digits
  , ipv4Block
  , word32x4ToWords
  , bitPower
  , blockSize
  , bitPower128
  , blockSize128
  , readsPrecOnParser
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Text.Appar.String as AP

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
octet = AP.try ((digits 1 2 #<*>#  digit 5  ) #<*># digits 0 5)
  <|>   AP.try ((digits 1 2 #<*># digits 0 4) #<*># digits 0 9)
  <|>   AP.try (( digit 1   #<*># digits 0 9) #<*># digits 0 9)
  <|>   AP.try ( digits 1 9 #<*># digits 0 9)
  <|>            digits 0 9

whitespace :: AP.Parser ()
whitespace = void $ many (AP.satisfy isSpace)

ipv4Address :: AP.Parser Word32
ipv4Address = fourOctetsToWord32
  <$> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*> (octet <* AP.char '.')
  <*>  octet

ipv4NetMask :: AP.Parser Word8
ipv4NetMask =  AP.try (digit 3   #<*># digits 0 2)
  <|>          AP.try (digit 2   #<*># digits 0 9)
  <|>          AP.try (digit 1   #<*># digits 0 9)
  <|>           digits 0 9

digit :: Int -> AP.Parser Word8
digit c      = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (== chr (c + 48))

digits :: Int -> Int -> AP.Parser Word8
digits c1 c2 = fromIntegral . (+ (-48)) . ord <$> AP.satisfy (\c -> c >= chr (c1 + 48) && c <= chr (c2 + 48))

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

readsPrecOnParser :: AP.Parser a -> Int -> String -> [(a, String)]
readsPrecOnParser p _ s = case AP.runParser (whitespace *> p) s of
    (Just a, r) -> [(a, r)]
    _           -> []
