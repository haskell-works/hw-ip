{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.Data.Network.Ip.Ip
  ( IpBlock(..)
  , isCanonical
  , firstIpAddress
  , lastIpAddress
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Validity
import Text.Read

import qualified Data.Bits                             as B
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6
import qualified HaskellWorks.Data.Network.Ip.Word128  as W
import qualified Text.ParserCombinators.ReadPrec       as RP

data IpBlock v = IpBlockV4 (V4.IpBlock v) | IpBlockV6 (V6.IpBlock v)
  deriving (Eq, Ord, Generic)

instance Show (IpBlock Unaligned) where
  showsPrec _ (IpBlockV4 a) = shows a
  showsPrec _ (IpBlockV6 a) = shows a

instance Read (IpBlock Unaligned) where
  readsPrec _ s =
    case readMaybe s :: Maybe (V4.IpBlock Unaligned) of
      Just ip -> [(IpBlockV4 ip, "")]

      Nothing ->
        case readMaybe s :: Maybe (V6.IpBlock Unaligned) of
          Just ipv6 -> [(IpBlockV6 ipv6, "")]
          Nothing   -> []

isCanonical :: IpBlock v -> Bool
isCanonical (IpBlockV4 b) = V4.isCanonical b
isCanonical (IpBlockV6 b) = V6.isCanonical b

firstIpAddress :: IpBlock Canonical -> (Word32, Word32, Word32, Word32)
firstIpAddress (IpBlockV4 v4Block)           = firstIpAddress (IpBlockV6 (V6.fromV4 v4Block))
-- firstIpAddress (IpBlockV6 (V6.IpBlock ip _)) = V6.words ip
firstIpAddress (IpBlockV6 (V6.IpBlock (V6.IpAddress ip) _)) = ip

lastIpAddress :: IpBlock Canonical -> (Word32, Word32, Word32, Word32)
lastIpAddress (IpBlockV4 ib) = (0, 0, 0xFFFF, V4.word (V4.lastIpAddress ib))
lastIpAddress (IpBlockV6 (V6.IpBlock (V6.IpAddress ip) (V6.IpNetMask msk))) =
    let (w1, w2, w3, w4) = ip
        lt = V6.masksIp $ fromIntegral msk
        w1' = w1 .|. (lt !! 0)
        w2' = w2 .|. (lt !! 1)
        w3' = w3 .|. (lt !! 2)
        w4' = w4 .|. (lt !! 3) in
      (w1', w2', w3', w4')
