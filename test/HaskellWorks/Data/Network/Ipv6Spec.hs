{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Data.Network.Ipv6Spec (spec) where

import HaskellWorks.Data.Network.Ip.SafeEnum
import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec


import qualified Data.Text                          as T
import qualified HaskellWorks.Data.Network.Ip.Ip    as V
import qualified HaskellWorks.Data.Network.Ip.Ipv4  as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6  as V6
import qualified HaskellWorks.Data.Network.Ip.Range as R
import qualified Data.Attoparsec.Text               as AP
import qualified Data.Text                          as T
import qualified HaskellWorks.Data.Network.Gen      as G
import qualified HaskellWorks.Data.Network.Ip.Ip    as V
import qualified HaskellWorks.Data.Network.Ip.Ipv4  as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6  as V6
import qualified HaskellWorks.Data.Network.Ip.Range as IR

import qualified Hedgehog.Gen                       as G
import qualified Hedgehog.Range                     as R
import qualified Text.Read                          as TR

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.Ipv6Spec" $ do
  describe "V6.IpBlock" $ do
    it "should implement show" $ requireTest $ do
      show (V6.IpBlock (V6.IpAddress (3, 3, 3, 0)) (V6.IpNetMask 96)) === "0:3:0:3:0:3::/96"

    it "should implement firstAddress/lastAddress" $ requireTest $ do
      V.firstIpAddress (V.IpBlockV4 (V4.IpBlock (V4.IpAddress   0xff000000) (V4.IpNetMask  8)))  === (0, 0, 0xFFFF, 0xFF000000)
      V.firstIpAddress (V.IpBlockV6 (V6.IpBlock (V6.IpAddress (4, 4, 0, 0)) (V6.IpNetMask 33)))  === (4, 4, 0, 0)
      V.lastIpAddress  (V.IpBlockV4 (V4.IpBlock (V4.IpAddress   0xff000000) (V4.IpNetMask  8)))  === (0 , 0 , 0xFFFF , 0xFFFFFFFF)
      V.lastIpAddress  (V.IpBlockV6 (V6.IpBlock (V6.IpAddress (4, 4, 0, 0)) (V6.IpNetMask 33)))  === (4 , 0x7FFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF)

    it "should implement read" $ requireTest $ do
      read "1:2:3:4::"      === V6.IpAddress (0x10002,0x30004,0,0)
      read "1:2:3:4::/127"  === V6.IpBlock @Unaligned (V6.IpAddress (0x10002   , 0x30004 , 0, 0)) (V6.IpNetMask 127)
      read "1234::/16"      === V6.IpBlock @Unaligned (V6.IpAddress (0x12340000, 0       , 0, 0)) (V6.IpNetMask  16)
      read "12:34::/32"     === V6.IpBlock @Unaligned (V6.IpAddress (0x120034  , 0       , 0, 0)) (V6.IpNetMask  32)

    it "should parse what it has shown" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      m <- forAll $ G.word8 $ R.linear 0 128
      let addr = V6.IpBlock (V6.IpAddress (a, 0, 0, 0)) (V6.IpNetMask 1)
      V6.parseIpBlock (T.pack (show addr)) === Right addr

    it "should support enum" $ require $ property $ do
      boundedPred (V6.IpAddress (32, 32, 32, 32)) === V6.IpAddress (32, 32, 32, 31)
      boundedSucc (V6.IpAddress (32, 32, 32, 32)) === V6.IpAddress (32, 32, 32, 33)
      boundedPred (V6.IpAddress (0, 0, 0, 0xffffffff)) === V6.IpAddress (0, 0, 0, 0xfffffffe)
      boundedSucc (V6.IpAddress (0, 0, 0, 0xffffffff)) === V6.IpAddress (0, 0, 1, 0)
      boundedSucc (V6.IpAddress (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) === V6.IpAddress (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)
      boundedPred (V6.IpAddress (0, 0, 0, 0)) === V6.IpAddress (0, 0, 0, 0)

  it "should convert ::/128 to ranges" $ requireTest $ do
    V6.blockToRange (V6.IpBlock (V6.IpAddress (0, 0, 0, 0)) (V6.IpNetMask 128)) === R.Range (V6.IpAddress (0, 0, 0, 0)) (V6.IpAddress (0, 0, 0, 0))

  it "should convert 1234::/64 to ranges" $ requireTest $ do
    V6.blockToRange (V6.IpBlock (V6.IpAddress (0x12340000, 0, 0, 0)) (V6.IpNetMask 64)) === R.Range (V6.IpAddress (0x12340000, 0, 0, 0)) (V6.IpAddress (0x12340000, 0, 0xffffffff, 0xffffffff))
      pred (V6.IpAddress (32, 32, 32, 32)) === V6.IpAddress (32, 32, 32, 31)
      succ (V6.IpAddress (32, 32, 32, 32)) === V6.IpAddress (32, 32, 32, 33)
      pred (V6.IpAddress (0, 0, 0, 0xffffffff)) === V6.IpAddress (0, 0, 0, 0xfffffffe)
      succ (V6.IpAddress (0, 0, 0, 0xffffffff)) === V6.IpAddress (0, 0, 1, 0)
      succ (V6.IpAddress (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) === V6.IpAddress (0, 0, 0, 0)
      pred (V6.IpAddress (0, 0, 0, 0)) === V6.IpAddress (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)

    it "block can be converted to range" $ require $ property $ do
      let b = read "1234::/16" :: V6.IpBlock
      V6.blockToRange b === IR.Range (read "1234::") (read "1234:ffff:ffff:ffff:ffff:ffff:ffff:ffff")

    it "block can be converted to range and back" $ require $ property $ do
      b <- forAll G.canonicalIpv6Block
      V6.splitIpRange (V6.blockToRange b) === (b, Nothing)

    it "ranges can be split" $ require $ property $ do
      V6.splitIpRange (IR.Range (read "::") (read "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")) === (V6.IpBlock (V6.IpAddress (0, 0, 0, 0)) (V6.IpNetMask 0), Nothing)
      V6.splitIpRange (IR.Range (read "::") (read "::88")) === (V6.IpBlock (V6.IpAddress (0, 0, 0, 0)) (V6.IpNetMask 121), Just (IR.Range (read "::80") (read "::88")))
      V6.splitIpRange (IR.Range (read "::3") (read "::88")) === (V6.IpBlock (V6.IpAddress (0, 0, 0, 3)) (V6.IpNetMask 128), Just (IR.Range (read "::4") (read "::88")))
      V6.splitIpRange (IR.Range (read "::127") (read "::129")) === (V6.IpBlock (V6.IpAddress (0, 0, 0, 0x127)) (V6.IpNetMask 128), Just (IR.Range (read "::128") (read "::129")))
