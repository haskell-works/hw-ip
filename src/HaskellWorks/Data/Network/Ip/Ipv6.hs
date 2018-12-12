{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module HaskellWorks.Data.Network.Ip.Ipv6
  ( Ipv6Address(..)
  , Ipv6NetMask(..)
  , Ipv6Block(..)
  , ipv4BlockToMappedIpv6Block
  , masksIpv6
  , isValidIpv6Block
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Generics.Product.Any
import Data.Maybe
import Data.Word
import GHC.Generics
import Prelude                   hiding (words)
import Text.Read

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.Bits                             as B
import qualified Data.IP                               as D
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as I4
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype Ipv6Address = Ipv6Address
  { words :: (Word32, Word32, Word32, Word32)
  } deriving (Eq, Ord, Generic)

instance Show Ipv6Address where
  showsPrec _ (Ipv6Address w) = shows (D.fromHostAddress6 w)

instance Read Ipv6Address where
  readsPrec :: Int -> String -> [(Ipv6Address, String)]
  readsPrec _ s =
    case readMaybe s :: Maybe D.IPv6 of
      Just ip -> [(Ipv6Address (D.toHostAddress6 ip), "")]
      Nothing -> []

newtype Ipv6NetMask = Ipv6NetMask
  { word :: Word8
  } deriving (Enum, Eq, Ord, Show, Generic)

instance Read Ipv6NetMask where
  readsPrec _ s =
    case Ipv6NetMask <$> m of
      Just maskv6 -> [(maskv6, "")]
      Nothing     -> []
    where
      m = mfilter (\a -> a >= 0 && a <= 128) (readMaybe s)

data Ipv6Block = Ipv6Block
  { base :: !Ipv6Address
  , mask :: !Ipv6NetMask
  } deriving (Eq, Ord, Generic)

instance Read Ipv6Block where
  readsPrec _ s =
    case T.unpack <$> T.split (== '/') (T.pack s) of
      [addr, mask] ->
        case readMaybe addr :: Maybe Ipv6Address of
          Just ipv6 ->
            case readMaybe mask of
              Just maskv6 ->
                let i6b = Ipv6Block ipv6 maskv6 in
                  [(i6b, "") | isValidIpv6Block i6b]
              Nothing     -> []
          Nothing -> []
      _ -> []

instance Show Ipv6Block where
  showsPrec _ (Ipv6Block b (Ipv6NetMask m))  = shows b . ('/':) . shows m

masksIpv6 :: Word8 -> [Word32]
masksIpv6 m =
  let e = 0xFFFFFFFF :: Word32
      -- bits: number of bits which should be 1
      maskValue bits = e `shiftR` (32 - bits) in
    if m < 32 then
      [maskValue (32 - fromIntegral m), e, e, e]
    else if m < 64 then
      [0, maskValue (64 - fromIntegral m), e, e]
    else if m < 96 then
      [0, 0, maskValue (96 - fromIntegral m), e]
    else if m < 128 then
      [0, 0, 0, maskValue (128 - fromIntegral m)]
    else
      [0, 0, 0, 0]

isValidIpv6Block :: Ipv6Block -> Bool
isValidIpv6Block (Ipv6Block b (Ipv6NetMask m)) =
  let lt = masksIpv6 m
      ipv6 = I.word32x4ToWords (words b) in
    ipv6 == zipWith (.&.) ipv6 (zipWith xor ipv6 lt)

ipv4BlockToMappedIpv6Block :: I4.Ipv4Block -> Ipv6Block
ipv4BlockToMappedIpv6Block (I4.Ipv4Block b m) =
  -- RFC-4291, "IPv4-Mapped IPv6 Address"
  Ipv6Block (Ipv6Address (0, 0, 0xFFFF, I4.word b)) (Ipv6NetMask (96 + I4.word8 m))
