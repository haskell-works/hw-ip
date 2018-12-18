module HaskellWorks.Data.Network.Gen
  ( canonicalIpv4Block
  ) where

import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Validity
import Hedgehog

import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R

canonicalIpv4Block :: MonadGen m => m (V4.IpBlock Canonical)
canonicalIpv4Block = do
  m <- G.word8   (R.linear 0 32)
  let p = fromIntegral (32 - m)
  w <- G.word32  (R.linear 0 ((0xffffffff .>. p) .<. p))
  return (V4.IpBlock (V4.IpAddress (w .<. p)) (V4.IpNetMask m))
