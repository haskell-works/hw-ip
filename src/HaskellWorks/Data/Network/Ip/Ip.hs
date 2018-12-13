{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module HaskellWorks.Data.Network.Ip.Ip
  ( IpBlock(..)
  , isValidIpBlock
  , firstAddress
  , lastAddress
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
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as I4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as I6
import qualified Text.ParserCombinators.ReadPrec       as RP

data IpBlock = IpBlockV4 I4.Ipv4Block | IpBlockV6 I6.Ipv6Block
  deriving (Eq, Ord, Generic)

instance Show IpBlock where
  showsPrec _ (IpBlockV4 a) = shows a
  showsPrec _ (IpBlockV6 a) = shows a

instance Read IpBlock where
  readsPrec _ s =
    case readMaybe s :: Maybe I4.Ipv4Block of
      Just ipv4 -> [(IpBlockV4 ipv4, "")]

      Nothing ->
        case readMaybe s :: Maybe I6.Ipv6Block of
          Just ipv6 -> [(IpBlockV6 ipv6, "")]
          Nothing   -> []

isValidIpBlock :: IpBlock -> Bool
isValidIpBlock (IpBlockV4 b) = I4.isValidIpv4Block b
isValidIpBlock (IpBlockV6 b) = I6.isValidIpv6Block b

firstAddress :: IpBlock -> (Word32, Word32, Word32, Word32)
firstAddress (IpBlockV4 i4b)                 = firstAddress (IpBlockV6 (I6.ipv4BlockToMappedIpv6Block i4b))
firstAddress (IpBlockV6 (I6.Ipv6Block ip _)) = I6.words ip

lastAddress :: IpBlock -> (Word32, Word32, Word32, Word32)
lastAddress (IpBlockV4 ib) = (0, 0, 0xFFFF, I4.word (I4.lastIpv4Address ib))
lastAddress (IpBlockV6 (I6.Ipv6Block ip (I6.Ipv6NetMask msk))) =
    let (w1, w2, w3, w4) = I6.words ip
        lt = I6.masksIpv6 $ fromIntegral msk
        w1' = w1 .|. (lt !! 0)
        w2' = w2 .|. (lt !! 1)
        w3' = w3 .|. (lt !! 2)
        w4' = w4 .|. (lt !! 3) in
      (w1', w2', w3', w4')
