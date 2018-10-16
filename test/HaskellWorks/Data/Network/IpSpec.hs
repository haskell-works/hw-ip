{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.IpSpec (spec) where

import HaskellWorks.Data.Network.Ip
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
  describe "Ipv4Address" $ do
    it "should implement show" $ require $ property $ do
      show (Ipv4Address 0x000000ff) === "0.0.0.255"
      show (Ipv4Address 0x0000ff00) === "0.0.255.0"
      show (Ipv4Address 0x00ff0000) === "0.255.0.0"
      show (Ipv4Address 0xff000000) === "255.0.0.0"
    it "should implement read" $ require $ property $ do
      read "1.2.3.4"      === Ipv4Address 0x01020304
      read "10.20.30.40"  === Ipv4Address 0x0a141e28
      read "1.2.3.12"     === Ipv4Address 0x0102030c
      read "1.2.3.160"    === Ipv4Address 0x010203a0
  describe "Ipv4Block" $ do
    it "should implement show" $ require $ property $ do
      show (Ipv4Block (Ipv4Address 0x000000ff) (Ipv4NetMask 32)) === "0.0.0.255/32"
      show (Ipv4Block (Ipv4Address 0x0000ff00) (Ipv4NetMask 32)) === "0.0.255.0/32"
      show (Ipv4Block (Ipv4Address 0x00ff0000) (Ipv4NetMask 32)) === "0.255.0.0/32"
      show (Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 32)) === "255.0.0.0/32"
      show (Ipv4Block (Ipv4Address 0x000000ff) (Ipv4NetMask 16)) === "0.0.0.255/16"
      show (Ipv4Block (Ipv4Address 0x0000ff00) (Ipv4NetMask 16)) === "0.0.255.0/16"
      show (Ipv4Block (Ipv4Address 0x00ff0000) (Ipv4NetMask 16)) === "0.255.0.0/16"
      show (Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 16)) === "255.0.0.0/16"
      show (firstIpv4Address $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 8))  === "255.0.0.0"
      show (lastIpv4Address  $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 8))  === "255.255.255.255"
      show (firstIpv4Address $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 16)) === "255.0.0.0"
      show (lastIpv4Address  $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 16)) === "255.0.255.255"
      show (firstIpv4Address $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 24)) === "255.0.0.0"
      show (lastIpv4Address  $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 24)) === "255.0.0.255"
      show (firstIpv4Address $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 32)) === "255.0.0.0"
      show (lastIpv4Address  $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 32)) === "255.0.0.0"
      show (firstIpv4Address $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 21)) === "255.0.0.0"
      show (lastIpv4Address  $ Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 21)) === "255.0.7.255"
    it "should implement splitBlock" $ require $ property $ do
      splitBlock (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32)) === Nothing
      splitBlock (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 31)) === Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32), Ipv4Block (Ipv4Address 0x00000001) (Ipv4NetMask 32))
      splitBlock (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 30)) === Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 31), Ipv4Block (Ipv4Address 0x00000002) (Ipv4NetMask 31))
      splitBlock (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  0)) === Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  1), Ipv4Block (Ipv4Address 0x80000000) (Ipv4NetMask  1))
    it "should implement blockSize" $ require $ property $ do
      blockSize (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32)) === 1
      blockSize (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  0)) === 0x100000000
