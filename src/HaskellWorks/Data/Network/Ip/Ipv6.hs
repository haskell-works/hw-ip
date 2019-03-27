{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module HaskellWorks.Data.Network.Ip.Ipv6
  ( IpAddress(..)
  , IpNetMask(..)
  , IpBlock(..)
  , fromIpv4
  , fromIpv4Block
  , fromV4
  , parseIpBlock
  , masksIp
  , showIpAddress
  , showsIpAddress
  , tshowIpAddress
  , tshowIpBlock
  , firstIpAddress
  , lastIpAddress
  , rangeToBlocks
  , rangeToBlocksDL
  , blockToRange
  , isCanonical
  , canonicaliseIpBlock
  , splitIpRange
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.SafeEnum
import HaskellWorks.Data.Network.Ip.Validity
import Prelude                               hiding (words)
import Text.Read

import qualified Data.Bits                             as B
import qualified Data.IP                               as D
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Word128  as W

newtype IpAddress = IpAddress W.Word128 deriving (Enum, Eq, Ord, Bounded, Generic, SafeEnum)

instance Show IpAddress where
  showsPrec _ (IpAddress w) = shows (D.fromHostAddress6 w)

instance Read IpAddress where
  readsPrec :: Int -> String -> [(IpAddress, String)]
  readsPrec _ s =
    case readMaybe s :: Maybe D.IPv6 of
      Just ip -> [(IpAddress (D.toHostAddress6 ip), "")]
      Nothing -> []

newtype IpNetMask = IpNetMask
  { word :: Word8
  } deriving (Enum, Eq, Ord, Show, Generic)

instance Bounded IpNetMask where
  minBound = IpNetMask 0
  maxBound = IpNetMask 128

instance Read IpNetMask where
  readsPrec _ s =
    case IpNetMask <$> m of
      Just maskv6 -> [(maskv6, "")]
      Nothing     -> []
    where
      m = mfilter (\a -> a >= 0 && a <= 128) (readMaybe s)

data IpBlock v = IpBlock
  { base :: !IpAddress
  , mask :: !IpNetMask
  } deriving (Eq, Ord, Bounded, Generic)

instance Read (IpBlock Unaligned) where
  readsPrec _ s =
    case T.unpack <$> T.split (== '/') (T.pack s) of
      [addr, msk] ->
        case readMaybe addr :: Maybe IpAddress of
          Just ipv6 ->
            case readMaybe msk of
              Just mskv6 ->
                let i6b = IpBlock ipv6 mskv6 in
                  [(i6b, "") | isCanonical i6b]
              Nothing     -> []
          Nothing -> []
      [addr] ->
        case readMaybe addr :: Maybe IpAddress of
          Just ipv6 -> let i6b = IpBlock ipv6 (IpNetMask 128) in [(i6b, "") | isCanonical i6b]
          Nothing   -> []
      _ -> []

instance Show (IpBlock v) where
  showsPrec _ (IpBlock b (IpNetMask m))  = shows b . ('/':) . shows m

parseIpBlock :: T.Text -> Either T.Text (IpBlock Unaligned)
parseIpBlock t =
  case T.unpack <$> T.split (== '/') t of
    [addr, msk] ->
      case readMaybe addr :: Maybe IpAddress of
        Just ipv6 ->
          case readMaybe msk of
            Just mskv6 -> Right $ IpBlock ipv6 mskv6
            Nothing    -> Left "cannot read mask"
        Nothing -> Left "cannot read addr"
    _ -> Left "invalid input string"

showsIpAddress :: IpAddress -> String -> String
showsIpAddress (IpAddress w) = shows (D.fromHostAddress6 w)

showIpAddress :: IpAddress -> String
showIpAddress ipAddress = showsIpAddress ipAddress ""

tshowIpAddress :: IpAddress -> T.Text
tshowIpAddress = T.pack . showIpAddress

showsIpBlock :: IpBlock v -> String -> String
showsIpBlock (IpBlock b (IpNetMask m)) = shows b . ('/':) . shows m

showIpBlock :: IpBlock v -> String
showIpBlock ipBlock = showsIpBlock ipBlock ""

tshowIpBlock :: IpBlock v -> T.Text
tshowIpBlock = T.pack . showIpBlock

masksIp :: Word8 -> [Word32]
masksIp m =
  let e = 0xFFFFFFFF :: Word32
      -- bits: number of bits which should be 1
      maskValue bits = e `B.shiftR` (32 - bits) in
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

isCanonical :: IpBlock v -> Bool
isCanonical block@(IpBlock (IpAddress w) (IpNetMask m)) =
  let IpBlock (IpAddress cw) (IpNetMask cm) = canonicaliseIpBlock block
  in cw == w && cm == m

{-# DEPRECATED fromV4 "Deprecated due to poor naming. Use fromIpv4Block instead." #-}
fromV4 :: V4.IpBlock Canonical -> IpBlock v
fromV4 = fromIpv4Block

fromIpv4Block :: V4.IpBlock Canonical -> IpBlock v
fromIpv4Block (V4.IpBlock b m) =
  -- RFC-4291, "IPv4-Mapped IPv6 Address"
  IpBlock (fromIpv4 b) (IpNetMask (96 + V4.word8 m))

fromIpv4 :: V4.IpAddress -> IpAddress
fromIpv4 (V4.IpAddress w32) = IpAddress (0, 0, 0xFFFF, w32)

canonicaliseIpBlock :: IpBlock v -> IpBlock Canonical
canonicaliseIpBlock (IpBlock (IpAddress w) (IpNetMask m))
  = case zipWith (B..&.) ipv6 (zipWith B.xor ipv6 masks) of
    [nw1,nw2,nw3,nw4] -> IpBlock (IpAddress (nw1, nw2, nw3, nw4)) (IpNetMask m)
    _                 -> error "Very mal-formed IPv6. This should never happen."
  where
    masks = masksIp m
    ipv6 = I.word32x4ToWords w


firstIpAddress :: IpBlock Canonical -> IpAddress
firstIpAddress (IpBlock b _) = b

lastIpAddress :: IpBlock Canonical -> IpAddress
lastIpAddress (IpBlock (IpAddress b) (IpNetMask m)) = IpAddress (b + fromIntegral (I.blockSize128 m) - 1)

splitIpRange :: Range IpAddress -> (IpBlock Canonical, Maybe (Range IpAddress))
splitIpRange (Range (IpAddress a) (IpAddress z)) = (block, remainder)
  where bpOuter   = width - B.countLeadingZeros (z + 1 - a) - 1
        bpInner   = B.countTrailingZeros ((maxBound `B.shiftL` fromIntegral bpOuter) B..|. a)
        block     = IpBlock (IpAddress a) (IpNetMask (fromIntegral (width - bpInner)))
        hostMask  = B.complement (maxBound `B.shiftL` fromIntegral bpInner)
        remainder = if a + hostMask >= z
          then Nothing
          else Just (Range (IpAddress (a + hostMask + 1)) (IpAddress z))
        width = B.finiteBitSize a

rangeToBlocksDL :: Range IpAddress -> [IpBlock Canonical] -> [IpBlock Canonical]
rangeToBlocksDL r = do
  let (b, remainder) = splitIpRange r
  case remainder of
    Just rmd -> (b:) . rangeToBlocksDL rmd
    Nothing  -> (b:)

rangeToBlocks :: Range IpAddress -> [IpBlock Canonical]
rangeToBlocks r = rangeToBlocksDL r []

blockToRange :: IpBlock Canonical -> Range IpAddress
blockToRange b = uncurry Range $ bimap firstIpAddress lastIpAddress (b, b)

instance Contains (IpBlock Canonical) where
  contains l r = firstIpAddress l <= firstIpAddress r && lastIpAddress l >= lastIpAddress r
