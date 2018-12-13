{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HaskellWorks.Data.Network.Ip.Ipv4
  ( Ipv4Address(..)
  , Ipv4NetMask(..)
  , Ipv4Block(..)
  , isValidIpv4Block
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  , textToMaybeIpv4Address
  , parseIpv4Address
  , ipv4AddressToString
  , ipv4AddressToText
  , ipv4AddressToWords
  , firstIpv4Address
  , lastIpv4Address
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import Text.Read

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.Bits                             as B
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype Ipv4Address = Ipv4Address
  { word :: Word32
  } deriving (Enum, Eq, Ord, Generic)

instance Show Ipv4Address where
  showsPrec _ (Ipv4Address w) =
    shows ((w .>. 24) .&. 0xff) . ('.':) .
    shows ((w .>. 16) .&. 0xff) . ('.':) .
    shows ((w .>.  8) .&. 0xff) . ('.':) .
    shows ( w         .&. 0xff)

instance Read Ipv4Address where
  readsPrec :: Int -> String -> [(Ipv4Address, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Address) (T.pack s) of
    Just result -> case result of
      AP.Done i r   -> [(Ipv4Address r, T.unpack i)]
      AP.Partial _  -> []
      AP.Fail a b c -> []
    Nothing -> []

newtype Ipv4NetMask = Ipv4NetMask
  { word8 :: Word8
  } deriving (Enum, Eq, Ord, Show, Generic)

data Ipv4Block = Ipv4Block
  { base :: !Ipv4Address
  , mask :: !Ipv4NetMask
  } deriving (Eq, Ord)

instance Show Ipv4Block where
  showsPrec _ (Ipv4Block b (Ipv4NetMask m)) = shows b . ('/':) . shows m

instance Read Ipv4Block where
  readsPrec :: Int -> String -> [(Ipv4Block, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Block) (T.pack s) of
    Just result -> case result of
      AP.Done i (a, m) ->
        case validIpv4Block $ Ipv4Block (Ipv4Address a) (Ipv4NetMask m) of
          Just b  -> [(b, T.unpack i)]
          Nothing -> []
      AP.Partial _    -> []
      AP.Fail a b c   -> []
    Nothing -> []

-- shift the address left by the amount of mask bits to reveal only host bits
-- if any bits left are non-zero, then the mask is not big enough
validIpv4Block :: Ipv4Block -> Maybe Ipv4Block
validIpv4Block b@(Ipv4Block (Ipv4Address word) (Ipv4NetMask mask)) =
  if word `B.shiftL` fromIntegral mask `B.xor` 0 == 0
    then pure b
    else Nothing

isValidIpv4Block :: Ipv4Block -> Bool
isValidIpv4Block = isJust . validIpv4Block

firstIpv4Address :: Ipv4Block -> Ipv4Address
firstIpv4Address (Ipv4Block base _) = base

lastIpv4Address :: Ipv4Block -> Ipv4Address
lastIpv4Address b@(Ipv4Block (Ipv4Address base) _) = Ipv4Address (base + fromIntegral (blockSize b) - 1)

bitPower :: Ipv4NetMask -> Word64
bitPower (Ipv4NetMask m) = fromIntegral (32 - m)

isCanonical :: Ipv4Block -> Bool
isCanonical (Ipv4Block (Ipv4Address b) m) = ((b .>. bitPower m) .<. bitPower m) == b

splitBlock :: Ipv4Block -> Maybe (Ipv4Block, Ipv4Block)
splitBlock (Ipv4Block (Ipv4Address b) (Ipv4NetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = Ipv4NetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) .>. fromIntegral (m + 1))
          in  Just
              ( Ipv4Block (Ipv4Address  b     ) halfMask
              , Ipv4Block (Ipv4Address (b + c)) halfMask
              )
    else  Nothing

blockSize :: Ipv4Block -> Int
blockSize (Ipv4Block _ m) = 2 ^ bitPower m

textToMaybeIpv4Address :: T.Text -> Maybe Ipv4Address
textToMaybeIpv4Address t = AP.maybeResult =<< AP.parseWith (return mempty) parseIpv4Address t

ipv4AddressToString :: Ipv4Address -> String
ipv4AddressToString = show

ipv4AddressToText :: Ipv4Address -> T.Text
ipv4AddressToText = T.pack . ipv4AddressToString

ipv4AddressToWords :: Ipv4Address -> (Word8, Word8, Word8, Word8)
ipv4AddressToWords (Ipv4Address w) =
  ( fromIntegral (w .>. 24) .&. 0xff
  , fromIntegral (w .>. 16) .&. 0xff
  , fromIntegral (w .>.  8) .&. 0xff
  , fromIntegral (w         .&. 0xff)
  )

parseIpv4Address :: AP.Parser Ipv4Address
parseIpv4Address = Ipv4Address <$> I.ipv4Address
