{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HaskellWorks.Data.Network.Ip.Ipv4
  ( IpAddress(..)
  , IpNetMask(..)
  , IpBlock(..)
  , isValidIpBlock
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  , textToMaybeIpAddress
  , parseIpAddress
  , ipAddressToString
  , ipAddressToText
  , ipAddressToWords
  , firstIpAddress
  , lastIpAddress
  , canonicaliseIpBlock
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

newtype IpAddress = IpAddress
  { word :: Word32
  } deriving (Enum, Eq, Ord, Generic)

instance Show IpAddress where
  showsPrec _ (IpAddress w) =
    shows ((w .>. 24) .&. 0xff) . ('.':) .
    shows ((w .>. 16) .&. 0xff) . ('.':) .
    shows ((w .>.  8) .&. 0xff) . ('.':) .
    shows ( w         .&. 0xff)

instance Read IpAddress where
  readsPrec :: Int -> String -> [(IpAddress, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Address) (T.pack s) of
    Just result -> case result of
      AP.Done i r   -> [(IpAddress r, T.unpack i)]
      AP.Partial _  -> []
      AP.Fail a b c -> []
    Nothing -> []

newtype IpNetMask = IpNetMask
  { word8 :: Word8
  } deriving (Enum, Eq, Ord, Show, Generic)

data IpBlock = IpBlock
  { base :: !IpAddress
  , mask :: !IpNetMask
  } deriving (Eq, Ord, Generic)

instance Show IpBlock where
  showsPrec _ (IpBlock b (IpNetMask m)) = shows b . ('/':) . shows m

instance Read IpBlock where
  readsPrec :: Int -> String -> [(IpBlock, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Block) (T.pack s) of
    Just result -> case result of
      AP.Done i (a, m) -> [(IpBlock (IpAddress a) (IpNetMask m), T.unpack i)]
      AP.Partial _     -> []
      AP.Fail a b c    -> []
    Nothing -> []

-- | A valid block must have all host-bits set to zero after the mask is applied
isValidIpBlock :: IpBlock -> Bool
isValidIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = word .<. fromIntegral mask == 0

-- | Canonicalise the block by zero-ing out the host bits
canonicaliseIpBlock :: IpBlock -> IpBlock
canonicaliseIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = IpBlock (IpAddress newWord) (IpNetMask mask)
  where bp = fromIntegral (32 - mask)
        newWord = (word .>. bp) .<. bp

firstIpAddress :: IpBlock -> IpAddress
firstIpAddress (IpBlock base _) = base

lastIpAddress :: IpBlock -> IpAddress
lastIpAddress b@(IpBlock (IpAddress base) _) = IpAddress (base + fromIntegral (blockSize b) - 1)

bitPower :: IpNetMask -> Word64
bitPower (IpNetMask m) = fromIntegral (32 - m)

isCanonical :: IpBlock -> Bool
isCanonical (IpBlock (IpAddress b) m) = ((b .>. bitPower m) .<. bitPower m) == b

splitBlock :: IpBlock -> Maybe (IpBlock, IpBlock)
splitBlock (IpBlock (IpAddress b) (IpNetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = IpNetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) .>. fromIntegral (m + 1))
          in  Just
              ( IpBlock (IpAddress  b     ) halfMask
              , IpBlock (IpAddress (b + c)) halfMask
              )
    else  Nothing

blockSize :: IpBlock -> Int
blockSize (IpBlock _ m) = 2 ^ bitPower m

textToMaybeIpAddress :: T.Text -> Maybe IpAddress
textToMaybeIpAddress t = AP.maybeResult =<< AP.parseWith (return mempty) parseIpAddress t

ipAddressToString :: IpAddress -> String
ipAddressToString = show

ipAddressToText :: IpAddress -> T.Text
ipAddressToText = T.pack . ipAddressToString

ipAddressToWords :: IpAddress -> (Word8, Word8, Word8, Word8)
ipAddressToWords (IpAddress w) =
  ( fromIntegral (w .>. 24) .&. 0xff
  , fromIntegral (w .>. 16) .&. 0xff
  , fromIntegral (w .>.  8) .&. 0xff
  , fromIntegral (w         .&. 0xff)
  )

parseIpAddress :: AP.Parser IpAddress
parseIpAddress = IpAddress <$> I.ipv4Address
