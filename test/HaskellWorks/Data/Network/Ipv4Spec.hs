{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Ipv4Spec (spec) where

import HaskellWorks.Data.Network.Ip.Internal
import HaskellWorks.Data.Network.Ip.Ipv4
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.Text              as AP
import qualified Data.List                         as DL
import qualified Data.Text                         as T
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R
import qualified Text.Read                         as TR

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.Ipv4Spec" $ do
  describe "octet" $ do
    it "should go from 0-255" $ require $ property $ do
      b <- forAll $ G.word8 R.constantBounded
      AP.parseOnly octet (T.pack . show $ b) === Right b

  describe "V4.IpAddress" $ do
    it "should implement show" $ requireTest $ do
      show (V4.IpAddress 0x000000ff) === "0.0.0.255"
      show (V4.IpAddress 0x0000ff00) === "0.0.255.0"
      show (V4.IpAddress 0x00ff0000) === "0.255.0.0"
      show (V4.IpAddress 0xff000000) === "255.0.0.0"

    it "should implement read" $ requireTest $ do
      read "1.2.3.4"      === V4.IpAddress 0x01020304
      read "10.20.30.40"  === V4.IpAddress 0x0a141e28
      read "1.2.3.12"     === V4.IpAddress 0x0102030c
      read "1.2.3.160"    === V4.IpAddress 0x010203a0

    it "should be possible to extract the octets" $ requireTest $ do
      V4.ipAddressToWords (V4.IpAddress 0x01020304) === (1, 2, 3, 4)

  describe "V4.IpBlock" $ do
    it "should implement show" $ requireTest $ do
      show (V4.IpBlock (V4.IpAddress 0x000000ff) (V4.IpNetMask 32)) === "0.0.0.255/32"
      show (V4.IpBlock (V4.IpAddress 0x0000ff00) (V4.IpNetMask 32)) === "0.0.255.0/32"
      show (V4.IpBlock (V4.IpAddress 0x00ff0000) (V4.IpNetMask 32)) === "0.255.0.0/32"
      show (V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 32)) === "255.0.0.0/32"
      show (V4.IpBlock (V4.IpAddress 0x000000ff) (V4.IpNetMask 16)) === "0.0.0.255/16"
      show (V4.IpBlock (V4.IpAddress 0x0000ff00) (V4.IpNetMask 16)) === "0.0.255.0/16"
      show (V4.IpBlock (V4.IpAddress 0x00ff0000) (V4.IpNetMask 16)) === "0.255.0.0/16"
      show (V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 16)) === "255.0.0.0/16"
      show (V4.firstIpAddress $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 8))  === "255.0.0.0"
      show (V4.lastIpAddress  $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 8))  === "255.255.255.255"
      show (V4.firstIpAddress $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 16)) === "255.0.0.0"
      show (V4.lastIpAddress  $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 16)) === "255.0.255.255"
      show (V4.firstIpAddress $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 24)) === "255.0.0.0"
      show (V4.lastIpAddress  $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 24)) === "255.0.0.255"
      show (V4.firstIpAddress $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 32)) === "255.0.0.0"
      show (V4.lastIpAddress  $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 32)) === "255.0.0.0"
      show (V4.firstIpAddress $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 21)) === "255.0.0.0"
      show (V4.lastIpAddress  $ V4.IpBlock (V4.IpAddress 0xff000000) (V4.IpNetMask 21)) === "255.0.7.255"

    it "should implement read" $ requireTest $ do
      read "1.0.0.0/8"  === V4.IpBlock (V4.IpAddress 0x01000000) (V4.IpNetMask 8)
      read "1.2.0.0/16" === V4.IpBlock (V4.IpAddress 0x01020000) (V4.IpNetMask 16)
      read "1.2.3.4/32" === V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 32)

    it "should implement splitBlock" $ requireTest $ do
      V4.splitBlock (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 32)) === Nothing
      V4.splitBlock (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 31)) === Just (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 32), V4.IpBlock (V4.IpAddress 0x00000001) (V4.IpNetMask 32))
      V4.splitBlock (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 30)) === Just (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 31), V4.IpBlock (V4.IpAddress 0x00000002) (V4.IpNetMask 31))
      V4.splitBlock (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask  0)) === Just (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask  1), V4.IpBlock (V4.IpAddress 0x80000000) (V4.IpNetMask  1))

    it "should implement blockSize" $ requireTest $ do
      V4.blockSize (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask 32)) === 1
      V4.blockSize (V4.IpBlock (V4.IpAddress 0x00000000) (V4.IpNetMask  0)) === 0x100000000

    it "should validate masks" $ requireTest $ do
      (read "1.2.3.4/8"  :: V4.IpBlock) === V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask  8)
      (read "1.2.3.4/0"  :: V4.IpBlock) === V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask  0)
      (read "1.2.3.4/32" :: V4.IpBlock) === V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 32)

    it "should canonicalise block" $ requireTest $ do
      V4.canonicaliseIpBlock (V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 32)) === V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 32)
      V4.canonicaliseIpBlock (V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 24)) === V4.IpBlock (V4.IpAddress 0x01020300) (V4.IpNetMask 24)
      V4.canonicaliseIpBlock (V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 16)) === V4.IpBlock (V4.IpAddress 0x01020000) (V4.IpNetMask 16)
      V4.canonicaliseIpBlock (V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask  8)) === V4.IpBlock (V4.IpAddress 0x01000000) (V4.IpNetMask  8)

    it "should collapse blocks" $ requireTest $ do
      let ipblocks1 =    read <$> ["1.2.3.4/32", "4.3.2.1/32"]
      collapseIpBlocks ipblocks1 === ipblocks1
      let ipblocks2 = read <$> ["1.2.3.3/32", "1.2.3.0/32", "1.2.3.1/32", "1.2.3.2/32"]
      collapseIpBlocks (DL.sort ipblocks2) === (read <$> ["1.2.3.0/30"])
      let ipblocks3 = read <$> ["1.2.3.3/32", "1.2.3.0/32", "1.2.3.1/32", "1.2.3.2/32", "5.5.5.5/32"]
      collapseIpBlocks (DL.sort ipblocks3) === (read <$> ["1.2.3.0/30", "5.5.5.5/32"])

  describe "should split ranges" $ do
      it "0.0.0.0 - 0.0.0.0" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.0" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 32), Nothing)
      it "0.0.0.1 - 0.0.0.1" $ requireTest $ do
        let ip1 = read "0.0.0.1" :: V4.IpAddress
        let ip2 = read "0.0.0.1" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 32), Nothing)
      it "0.0.0.0 - 0.0.0.1" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.1" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 31), Nothing)
      it "0.0.0.0 - 0.0.0.2" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.2" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 31), Just (Range ip2 ip2))
      it "0.0.0.0 - 0.0.0.3" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.3" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 30), Nothing)
      it "0.0.0.0 - 0.0.0.4" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.4" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 30), Just (Range ip2 ip2))
      it "0.0.0.0 - 0.0.0.5" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.4" :: V4.IpAddress
        let ip3 = read "0.0.0.5" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 30), Just (Range ip2 ip3))
      it "0.0.0.0 - 0.0.0.5" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.4" :: V4.IpAddress
        let ip3 = read "0.0.0.6" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 30), Just (Range ip2 ip3))
      it "0.0.0.0 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.0" :: V4.IpAddress
        let ip2 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 29), Nothing)
      it "0.0.0.1 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.1" :: V4.IpAddress
        let ip2 = read "0.0.0.2" :: V4.IpAddress
        let ip3 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 32), Just (Range ip2 ip3))
      it "0.0.0.2 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.2" :: V4.IpAddress
        let ip2 = read "0.0.0.4" :: V4.IpAddress
        let ip3 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 31), Just (Range ip2 ip3))
      it "0.0.0.3 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.3" :: V4.IpAddress
        let ip2 = read "0.0.0.4" :: V4.IpAddress
        let ip3 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 32), Just (Range ip2 ip3))
      it "0.0.0.4 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.4" :: V4.IpAddress
        let ip2 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 30), Nothing)
      it "0.0.0.5 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.5" :: V4.IpAddress
        let ip2 = read "0.0.0.6" :: V4.IpAddress
        let ip3 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip3) === (V4.IpBlock ip1 (V4.IpNetMask 32), Just (Range ip2 ip3))
      it "0.0.0.6 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.6" :: V4.IpAddress
        let ip2 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 31), Nothing)
      it "0.0.0.7 - 0.0.0.7" $ requireTest $ do
        let ip1 = read "0.0.0.7" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip1) === (V4.IpBlock ip1 (V4.IpNetMask 32), Nothing)
      it "0.0.0.6 - 0.0.0.6" $ requireTest $ do
        let ip1 = read "0.0.0.6" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip1) === (V4.IpBlock ip1 (V4.IpNetMask 32), Nothing)
      it "255.255.255.255 - 255.255.255.255" $ requireTest $ do
        let ip1 = read "255.255.255.255" :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip1) === (V4.IpBlock ip1 (V4.IpNetMask 32), Nothing)
      it "128.0.0.0 - 255.255.255.255" $ requireTest $ do
        let ip1 = read "128.0.0.0"        :: V4.IpAddress
        let ip2 = read "255.255.255.255"  :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 1), Nothing)
      it "0.0.0.0 - 255.255.255.255" $ requireTest $ do
        let ip1 = read "0.0.0.0"          :: V4.IpAddress
        let ip2 = read "255.255.255.255"  :: V4.IpAddress
        V4.splitIpRange (Range ip1 ip2) === (V4.IpBlock ip1 (V4.IpNetMask 0), Nothing)
