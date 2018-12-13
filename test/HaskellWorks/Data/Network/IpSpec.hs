{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.IpSpec (spec) where

import Data.List
import HaskellWorks.Data.Network.Ip
import HaskellWorks.Data.Network.Ip.Internal
import HaskellWorks.Data.Network.Ip.Ipv4
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

<<<<<<< HEAD
import qualified Data.Attoparsec.Text              as AP
import qualified Data.Text                         as T
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R
import qualified Text.Read                         as TR
=======
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text            as T
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R
import qualified Text.Read            as TR
>>>>>>> collapse ipv4 addresses into blocks

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
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
      (TR.readMaybe "1.2.3.4/8"  :: Maybe V4.IpBlock) === Nothing
      (TR.readMaybe "1.2.3.4/0"  :: Maybe V4.IpBlock) === Nothing
      (TR.readMaybe "1.2.3.4/32" :: Maybe V4.IpBlock) === (Just $ V4.IpBlock (V4.IpAddress 0x01020304) (V4.IpNetMask 32))
      (TR.readMaybe "1.2.3.4/8"  :: Maybe Ipv4Block) === Nothing
      (TR.readMaybe "1.2.3.4/0"  :: Maybe Ipv4Block) === Nothing
      (TR.readMaybe "1.2.3.4/32" :: Maybe Ipv4Block) === (Just $ Ipv4Block (Ipv4Address 0x01020304) (Ipv4NetMask 32))

    it "should collapse blocks" $ require $ property $ do
      let ipv4blocks1 =    read <$> ["1.2.3.4/32", "4.3.2.1/32"]
      collapseIpv4Blocks ipv4blocks1 === ipv4blocks1
      let ipv4blocks2 = read <$> ["1.2.3.3/32", "1.2.3.0/32", "1.2.3.1/32", "1.2.3.2/32"]
      (collapseIpv4Blocks $ sort ipv4blocks2) === (read <$> ["1.2.3.0/30"])
