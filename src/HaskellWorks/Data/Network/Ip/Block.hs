{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module HaskellWorks.Data.Network.Ip.Block
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
import qualified HaskellWorks.Data.Network.Ip          as Z
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as I4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as I6
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype IpBlock = IpBlock
  { block :: Either I4.Ipv4Block I6.Ipv6Block
  } deriving (Eq, Ord, Generic)


instance Show IpBlock where
  showsPrec _ (IpBlock (Left ipv4Block))  = shows ipv4Block
  showsPrec _ (IpBlock (Right ipv6Block)) = shows ipv6Block

instance Read IpBlock where
  readsPrec _ s =
    case readMaybe s :: Maybe I4.Ipv4Block of
      Just ipv4 -> [(IpBlock (Left ipv4), "")]

      Nothing ->
        case readMaybe s :: Maybe I6.Ipv6Block of
          Just ipv6 -> [(IpBlock (Right ipv6), "")]
          Nothing   -> []

isValidIpBlock :: IpBlock -> Bool
isValidIpBlock (IpBlock (Left _))    = True -- TODO
isValidIpBlock (IpBlock (Right i6b)) = I6.isValidIpv6Block i6b

firstAddress :: IpBlock -> (Word32, Word32, Word32, Word32)
firstAddress (IpBlock (Left i4b))                  = firstAddress (IpBlock (Right $ I6.ipv4BlockToMappedIpv6Block i4b))
firstAddress (IpBlock (Right (I6.Ipv6Block ip _))) = I6.words ip

lastAddress :: IpBlock -> (Word32, Word32, Word32, Word32)
lastAddress (IpBlock (Left ib)) = (0, 0, 0xFFFF, I4.word (Z.lastIpv4Address ib))
lastAddress (IpBlock (Right (I6.Ipv6Block ip (I6.Ipv6NetMask msk)))) =
    let (w1, w2, w3, w4) = I6.words ip
        lt = I6.masksIpv6 $ fromIntegral msk
        w1' = w1 .|. (lt !! 0)
        w2' = w2 .|. (lt !! 1)
        w3' = w3 .|. (lt !! 2)
        w4' = w4 .|. (lt !! 3) in
      (w1', w2', w3', w4')
