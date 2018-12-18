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
  , fromV4
  , parseIpBlock
  , masksIp
  , isValidIpBlock
  , firstIpAddress
  , lastIpAddress
  , rangeToBlocks
  , rangeToBlocksDL
  , blockToRange
  ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Generics.Product.Any
import Data.Maybe
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.SafeEnum
import HaskellWorks.Data.Network.Ip.Validity
import Prelude                               hiding (words)
import Text.Read

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.Bits                             as B
import qualified Data.IP                               as D
import qualified Data.String                           as S
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype IpAddress = IpAddress
  { words :: (Word32, Word32, Word32, Word32)
  } deriving (Eq, Ord, Generic)

instance SafeEnum IpAddress where
  safeSucc (IpAddress (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) = Nothing
  safeSucc (IpAddress (a,          0xffffffff, 0xffffffff, 0xffffffff)) = Just (IpAddress (succ a, 0, 0, 0))
  safeSucc (IpAddress (a,                   b, 0xffffffff, 0xffffffff)) = Just (IpAddress (a, succ b, 0, 0))
  safeSucc (IpAddress (a,                   b,          c, 0xffffffff)) = Just (IpAddress (a, b, succ c, 0))
  safeSucc (IpAddress (a,                   b,          c,          d)) = Just (IpAddress (a, b, c, succ d))

  safePred (IpAddress (0, 0, 0, 0)) = Nothing
  safePred (IpAddress (a, 0, 0, 0)) = Just (IpAddress (    pred a, 0xffffffff, 0xffffffff, 0xffffffff))
  safePred (IpAddress (a, b, 0, 0)) = Just (IpAddress (         a,     pred b, 0xffffffff, 0xffffffff))
  safePred (IpAddress (a, b, c, 0)) = Just (IpAddress (         a,          b,     pred c, 0xffffffff))
  safePred (IpAddress (a, b, c, d)) = Just (IpAddress (         a,          b,          c,     pred d))

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
  } deriving (Eq, Ord, Generic)

instance Read (IpBlock Unaligned) where
  readsPrec _ s =
    case T.unpack <$> T.split (== '/') (T.pack s) of
      [addr, mask] ->
        case readMaybe addr :: Maybe IpAddress of
          Just ipv6 ->
            case readMaybe mask of
              Just maskv6 ->
                let i6b = IpBlock ipv6 maskv6 in
                  [(i6b, "") | isValidIpBlock i6b]
              Nothing     -> []
          Nothing -> []
      _ -> []

instance Show (IpBlock v) where
  showsPrec _ (IpBlock b (IpNetMask m))  = shows b . ('/':) . shows m

parseIpBlock :: T.Text -> Either T.Text (IpBlock Unaligned)
parseIpBlock t =
  case T.unpack <$> T.split (== '/') t of
    [addr, mask] ->
      case readMaybe addr :: Maybe IpAddress of
        Just ipv6 ->
          case readMaybe mask of
            Just maskv6 -> Right $ IpBlock ipv6 maskv6
            Nothing     -> Left "cannot read mask"
        Nothing -> Left "cannot read addr"
    _ -> Left "invalid input string"

masksIp :: Word8 -> [Word32]
masksIp m =
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

isValidIpBlock :: IpBlock v -> Bool
isValidIpBlock (IpBlock b (IpNetMask m)) =
  let lt = masksIp m
      ipv6 = I.word32x4ToWords (words b) in
    ipv6 == zipWith (.&.) ipv6 (zipWith xor ipv6 lt)

fromV4 :: V4.IpBlock Canonical -> IpBlock v
fromV4 (V4.IpBlock b m) =
  -- RFC-4291, "IPv4-Mapped IPv6 Address"
  IpBlock (IpAddress (0, 0, 0xFFFF, V4.word b)) (IpNetMask (96 + V4.word8 m))

intValue :: IpAddress -> Integer
intValue (IpAddress (a, b, c, d)) =
  let a' = fromIntegral a `shift` 96
      b' = fromIntegral b `shift` 64
      c' = fromIntegral c `shift` 32
      d' = fromIntegral d `shift` 00
  in a' .|. b' .|. c' .|. d'

ipValue :: Integer -> IpAddress
ipValue i =
  let i' = fromIntegral i :: Integer
      a  = fromIntegral (i' `B.shiftR` 96 B..&. 0xffffffff)
      b  = fromIntegral (i' `B.shiftR` 64 B..&. 0xffffffff)
      c  = fromIntegral (i' `B.shiftR` 32 B..&. 0xffffffff)
      d  = fromIntegral (i' `B.shiftR` 00 B..&. 0xffffffff)
  in IpAddress (a, b, c, d)

firstIpAddress :: IpBlock Canonical -> IpAddress
firstIpAddress (IpBlock base _) = base

lastIpAddress :: IpBlock Canonical -> IpAddress
lastIpAddress b@(IpBlock i@(IpAddress base) (IpNetMask m)) =
  ipValue $ intValue i + fromIntegral (I.blockSize128 m) - 1

rangeToBlocksDL :: Range IpAddress -> [IpBlock Canonical] -> [IpBlock Canonical]
rangeToBlocksDL = error "TODO implement rangeToBlocksDL"

rangeToBlocks :: Range IpAddress -> [IpBlock Canonical]
rangeToBlocks = error "TODO implement rangeToBlocks"

blockToRange :: IpBlock Canonical -> Range IpAddress
blockToRange b = uncurry Range $ bimap firstIpAddress lastIpAddress (b, b)
