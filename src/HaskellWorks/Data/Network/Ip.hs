{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Data.Network.Ip
  ( Z.Ipv4Address(Ipv4Address)
  , Z.Ipv4NetMask(Ipv4NetMask)
  , Z.Ipv4Block(Ipv4Block)
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  , textToMaybeIpv4Address
  , ipv4AddressToString
  , ipv4AddressToText
  , startIpv4Address
  , stopIpv4Address
  ) where

import Control.Monad
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Attoparsec.Text                     as AP
import qualified Data.Text                                as T
import qualified HaskellWorks.Data.Network.Ip.Internal    as I
import qualified HaskellWorks.Data.Network.Ip.Parser.Text as APT
import qualified HaskellWorks.Data.Network.Ip.Type        as Z

bitPower :: Z.Ipv4NetMask -> Word64
bitPower (Z.Ipv4NetMask m) = fromIntegral (32 - m)

isCanonical :: Z.Ipv4Block -> Bool
isCanonical (Z.Ipv4Block (Z.Ipv4Address b) m) = ((b .>. bitPower m) .<. bitPower m) == b

splitBlock :: Z.Ipv4Block -> Maybe (Z.Ipv4Block, Z.Ipv4Block)
splitBlock (Z.Ipv4Block (Z.Ipv4Address b) (Z.Ipv4NetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = Z.Ipv4NetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) .>. fromIntegral (m + 1))
          in  Just
              ( Z.Ipv4Block (Z.Ipv4Address  b     ) halfMask
              , Z.Ipv4Block (Z.Ipv4Address (b + c)) halfMask
              )
    else  Nothing

blockSize :: Z.Ipv4Block -> Int
blockSize (Z.Ipv4Block _ m) = 2 ^ bitPower m

startIpv4Address :: Z.Ipv4Block -> Z.Ipv4Address
startIpv4Address (Z.Ipv4Block base _) = base

stopIpv4Address :: Z.Ipv4Block -> Z.Ipv4Address
stopIpv4Address b@(Z.Ipv4Block (Z.Ipv4Address base) _) = Z.Ipv4Address (base + fromIntegral (blockSize b) - 1)

textToMaybeIpv4Address :: T.Text -> Maybe Z.Ipv4Address
textToMaybeIpv4Address t = join $ AP.maybeResult <$> AP.parseWith (return mempty) APT.ipv4Address t

ipv4AddressToString :: Z.Ipv4Address -> String
ipv4AddressToString = show

ipv4AddressToText :: Z.Ipv4Address -> T.Text
ipv4AddressToText = T.pack . ipv4AddressToString
