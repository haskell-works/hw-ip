{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Data.Network.Ipv4Spec (spec) where

import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Internal
import HaskellWorks.Data.Network.Ip.Ipv4
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.List                             as DL
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Gen         as G
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Ipv4     as I
import qualified HaskellWorks.Data.Network.Ip.Range    as I
import qualified Hedgehog.Gen                          as G
import qualified Hedgehog.Range                        as R
import qualified Text.Read                             as TR

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.Ipv4Spec" $ do
  describe "octet" $ do
    it "should go from 0-255" $ require $ property $ do
      b <- forAll $ G.word8 R.constantBounded
      AP.parseOnly octet (T.pack . show $ b) === Right b

  describe "I.IpAddress" $ do
    it "should implement show" $ requireTest $ do
      show (I.IpAddress 0x000000ff) === "0.0.0.255"
      show (I.IpAddress 0x0000ff00) === "0.0.255.0"
      show (I.IpAddress 0x00ff0000) === "0.255.0.0"
      show (I.IpAddress 0xff000000) === "255.0.0.0"

    it "should implement read" $ requireTest $ do
      read "1.2.3.4"      === I.IpAddress 0x01020304
      read "10.20.30.40"  === I.IpAddress 0x0a141e28
      read "1.2.3.12"     === I.IpAddress 0x0102030c
      read "1.2.3.160"    === I.IpAddress 0x010203a0

    it "should be possible to extract the octets" $ requireTest $ do
      I.ipAddressToWords (I.IpAddress 0x01020304) === (1, 2, 3, 4)

  describe "I.IpBlock" $ do
    it "should implement show" $ requireTest $ do
      show (I.IpBlock (I.IpAddress 0x000000ff) (I.IpNetMask 32)) === "0.0.0.255/32"
      show (I.IpBlock (I.IpAddress 0x0000ff00) (I.IpNetMask 32)) === "0.0.255.0/32"
      show (I.IpBlock (I.IpAddress 0x00ff0000) (I.IpNetMask 32)) === "0.255.0.0/32"
      show (I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 32)) === "255.0.0.0/32"
      show (I.IpBlock (I.IpAddress 0x000000ff) (I.IpNetMask 16)) === "0.0.0.255/16"
      show (I.IpBlock (I.IpAddress 0x0000ff00) (I.IpNetMask 16)) === "0.0.255.0/16"
      show (I.IpBlock (I.IpAddress 0x00ff0000) (I.IpNetMask 16)) === "0.255.0.0/16"
      show (I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 16)) === "255.0.0.0/16"
      show (I.firstIpAddress $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 8))  === "255.0.0.0"
      show (I.lastIpAddress  $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 8))  === "255.255.255.255"
      show (I.firstIpAddress $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 16)) === "255.0.0.0"
      show (I.lastIpAddress  $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 16)) === "255.0.255.255"
      show (I.firstIpAddress $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 24)) === "255.0.0.0"
      show (I.lastIpAddress  $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 24)) === "255.0.0.255"
      show (I.firstIpAddress $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 32)) === "255.0.0.0"
      show (I.lastIpAddress  $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 32)) === "255.0.0.0"
      show (I.firstIpAddress $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 21)) === "255.0.0.0"
      show (I.lastIpAddress  $ I.IpBlock (I.IpAddress 0xff000000) (I.IpNetMask 21)) === "255.0.7.255"

    it "should implement read" $ requireTest $ do
      read "1.0.0.0/8"  === I.IpBlock @Unaligned (I.IpAddress 0x01000000) (I.IpNetMask 8)
      read "1.2.0.0/16" === I.IpBlock @Unaligned (I.IpAddress 0x01020000) (I.IpNetMask 16)
      read "1.2.3.4/32" === I.IpBlock @Unaligned (I.IpAddress 0x01020304) (I.IpNetMask 32)

    it "should implement splitBlock" $ requireTest $ do
      I.splitBlock (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask 32)) === Nothing
      I.splitBlock (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask 31)) === Just (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask 32), I.IpBlock (I.IpAddress 0x00000001) (I.IpNetMask 32))
      I.splitBlock (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask 30)) === Just (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask 31), I.IpBlock (I.IpAddress 0x00000002) (I.IpNetMask 31))
      I.splitBlock (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask  0)) === Just (I.IpBlock (I.IpAddress 0x00000000) (I.IpNetMask  1), I.IpBlock (I.IpAddress 0x80000000) (I.IpNetMask  1))

    it "should implement blockSize" $ requireTest $ do
      I.blockSize 32 === 0x000000001
      I.blockSize  0 === 0x100000000

    it "should validate masks" $ requireTest $ do
      (read "1.2.3.4/8"  :: I.IpBlock Unaligned) === I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask  8)
      (read "1.2.3.4/0"  :: I.IpBlock Unaligned) === I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask  0)
      (read "1.2.3.4/32" :: I.IpBlock Unaligned) === I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask 32)

    it "should canonicalise block" $ requireTest $ do
      I.canonicaliseIpBlock (I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask 32)) === I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask 32)
      I.canonicaliseIpBlock (I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask 24)) === I.IpBlock (I.IpAddress 0x01020300) (I.IpNetMask 24)
      I.canonicaliseIpBlock (I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask 16)) === I.IpBlock (I.IpAddress 0x01020000) (I.IpNetMask 16)
      I.canonicaliseIpBlock (I.IpBlock (I.IpAddress 0x01020304) (I.IpNetMask  8)) === I.IpBlock (I.IpAddress 0x01000000) (I.IpNetMask  8)

    it "should collapse blocks" $ requireTest $ do
      let ipblocks1 = read @(IpBlock Canonical) <$> ["1.2.3.4/32", "4.3.2.1/32"]
      collapseIpBlocks ipblocks1 === ipblocks1
      let ipblocks2 = read @(IpBlock Canonical) <$> ["1.2.3.3/32", "1.2.3.0/32", "1.2.3.1/32", "1.2.3.2/32"]
      collapseIpBlocks (DL.sort ipblocks2) === (read @(IpBlock Canonical) <$> ["1.2.3.0/30"])
      let ipblocks3 = read @(IpBlock Canonical) <$> ["1.2.3.3/32", "1.2.3.0/32", "1.2.3.1/32", "1.2.3.2/32", "5.5.5.5/32"]
      collapseIpBlocks (DL.sort ipblocks3) === (read @(IpBlock Canonical) <$> ["1.2.3.0/30", "5.5.5.5/32"])

  describe "should split ranges" $ do
    it "0.0.0.0 - 0.0.0.0" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.0" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 32), Nothing)
    it "0.0.0.1 - 0.0.0.1" $ requireTest $ do
      let ip1 = read "0.0.0.1" :: I.IpAddress
      let ip2 = read "0.0.0.1" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 32), Nothing)
    it "0.0.0.0 - 0.0.0.1" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.1" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 31), Nothing)
    it "0.0.0.0 - 0.0.0.2" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.2" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 31), Just (Range ip2 ip2))
    it "0.0.0.0 - 0.0.0.3" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.3" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 30), Nothing)
    it "0.0.0.0 - 0.0.0.4" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.4" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 30), Just (Range ip2 ip2))
    it "0.0.0.0 - 0.0.0.5" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.4" :: I.IpAddress
      let ip3 = read "0.0.0.5" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 30), Just (Range ip2 ip3))
    it "0.0.0.0 - 0.0.0.5" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.4" :: I.IpAddress
      let ip3 = read "0.0.0.6" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 30), Just (Range ip2 ip3))
    it "0.0.0.0 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.0" :: I.IpAddress
      let ip2 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 29), Nothing)
    it "0.0.0.1 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.1" :: I.IpAddress
      let ip2 = read "0.0.0.2" :: I.IpAddress
      let ip3 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 32), Just (Range ip2 ip3))
    it "0.0.0.2 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.2" :: I.IpAddress
      let ip2 = read "0.0.0.4" :: I.IpAddress
      let ip3 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 31), Just (Range ip2 ip3))
    it "0.0.0.3 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.3" :: I.IpAddress
      let ip2 = read "0.0.0.4" :: I.IpAddress
      let ip3 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 32), Just (Range ip2 ip3))
    it "0.0.0.4 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.4" :: I.IpAddress
      let ip2 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 30), Nothing)
    it "0.0.0.5 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.5" :: I.IpAddress
      let ip2 = read "0.0.0.6" :: I.IpAddress
      let ip3 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip3) === (I.IpBlock ip1 (I.IpNetMask 32), Just (Range ip2 ip3))
    it "0.0.0.6 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.6" :: I.IpAddress
      let ip2 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 31), Nothing)
    it "0.0.0.7 - 0.0.0.7" $ requireTest $ do
      let ip1 = read "0.0.0.7" :: I.IpAddress
      I.splitIpRange (Range ip1 ip1) === (I.IpBlock ip1 (I.IpNetMask 32), Nothing)
    it "0.0.0.6 - 0.0.0.6" $ requireTest $ do
      let ip1 = read "0.0.0.6" :: I.IpAddress
      I.splitIpRange (Range ip1 ip1) === (I.IpBlock ip1 (I.IpNetMask 32), Nothing)
    it "255.255.255.255 - 255.255.255.255" $ requireTest $ do
      let ip1 = read "255.255.255.255" :: I.IpAddress
      I.splitIpRange (Range ip1 ip1) === (I.IpBlock ip1 (I.IpNetMask 32), Nothing)
    it "128.0.0.0 - 255.255.255.255" $ requireTest $ do
      let ip1 = read "128.0.0.0"        :: I.IpAddress
      let ip2 = read "255.255.255.255"  :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 1), Nothing)
    it "0.0.0.0 - 255.255.255.255" $ requireTest $ do
      let ip1 = read "0.0.0.0"          :: I.IpAddress
      let ip2 = read "255.255.255.255"  :: I.IpAddress
      I.splitIpRange (Range ip1 ip2) === (I.IpBlock ip1 (I.IpNetMask 0), Nothing)

  describe "should get blocks from ranges" $ do
    it "0.0.0.1 - 0.0.0.2" $ requireTest $ do
      I.rangeToBlocks (I.Range (I.IpAddress 0x000001) (I.IpAddress 0x000002)) === [ I.IpBlock (I.IpAddress 0x000001) (I.IpNetMask 32)
                                                                                  , I.IpBlock (I.IpAddress 0x000002) (I.IpNetMask 32)]
    it "102.36.48.28 - 102.36.48.255" $ requireTest $ do
      I.rangeToBlocks (I.Range (I.IpAddress 0x6624301c) (I.IpAddress 0x662430ff)) === [ I.IpBlock (I.IpAddress 0x6624301c) (I.IpNetMask 30)
                                                                                      , I.IpBlock (I.IpAddress 0x66243020) (I.IpNetMask 27)
                                                                                      , I.IpBlock (I.IpAddress 0x66243040) (I.IpNetMask 26)
                                                                                      , I.IpBlock (I.IpAddress 0x66243080) (I.IpNetMask 25)]

    it "102.36.48.2 - 102.36.48.8" $ requireTest $ do
      I.rangeToBlocks (I.Range (I.IpAddress 0x66243002) (I.IpAddress 0x66243008)) === [ I.IpBlock (I.IpAddress 0x66243002) (I.IpNetMask 31)
                                                                                      , I.IpBlock (I.IpAddress 0x66243004) (I.IpNetMask 30)
                                                                                      , I.IpBlock (I.IpAddress 0x66243008) (I.IpNetMask 32)]

  describe "should get blocks from ranges with difference lists" $ do
    it "0.0.0.1 - 0.0.0.2" $ requireTest $ do
      I.rangeToBlocksDL (I.Range (I.IpAddress 0x000001) (I.IpAddress 0x000002)) [] === [ I.IpBlock (I.IpAddress 0x000001) (I.IpNetMask 32)
                                                                                        , I.IpBlock (I.IpAddress 0x000002) (I.IpNetMask 32)]

  it "block can be converted to range and back" $ require $ property $ do
    b <- forAll G.canonicalIpv4Block
    I.splitIpRange (I.blockToRange b) === (b, Nothing)
