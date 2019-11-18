module HaskellWorks.Data.Network.Gen
  ( canonicalIpv4Block
  , canonicalIpv6Block
  ) where

import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Validity
import Hedgehog

import qualified Data.Bits                         as B
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R

canonicalIpv4Block :: MonadGen m => m (V4.IpBlock Canonical)
canonicalIpv4Block = do
  m <- G.word8   (R.linear 0 32)
  let p = fromIntegral (32 - m)
  w <- G.word32  (R.linear 0 ((0xffffffff .>. p) .<. p))
  return (V4.IpBlock (V4.IpAddress (w .<. p)) (V4.IpNetMask m))

canonicalIpv6Block :: MonadGen m => m (V6.IpBlock Canonical)
canonicalIpv6Block = do
  m <- G.word8   (R.linear 0 128)
  let p = fromIntegral (128 - m)
  w128 <- (,,,) <$> G.word32 (R.linear 0 0xffffffff)
                <*> G.word32 (R.linear 0 0xffffffff)
                <*> G.word32 (R.linear 0 0xffffffff)
                <*> G.word32 (R.linear 0 0xffffffff)
  return (V6.IpBlock (V6.IpAddress (w128 `B.shift` p)) (V6.IpNetMask m))
