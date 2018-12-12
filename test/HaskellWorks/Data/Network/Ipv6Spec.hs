{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Ipv6Spec (spec) where

import HaskellWorks.Data.Network.Ip
import HaskellWorks.Data.Network.Ip.Block
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
  describe "Ipv6Block" $ do
    it "should implement show" $ require $ property $ do
      show (Ipv6Block (Ipv6Address (3, 3, 3, 0)) (Ipv6NetMask 96)) === "0:3:0:3:0:3::/96"

    it "should implement firstAddress/lastAddress" $ require $ property $ do
      (firstAddress $ IpBlockV4 (Ipv4Block (Ipv4Address   0xff000000) (Ipv4NetMask  8)))  === (0, 0, 0xFFFF, 0xFF000000)
      (firstAddress $ IpBlockV6 (Ipv6Block (Ipv6Address (4, 4, 0, 0)) (Ipv6NetMask 33)))  === (4, 4, 0, 0)
      (lastAddress  $ IpBlockV4 (Ipv4Block (Ipv4Address   0xff000000) (Ipv4NetMask  8)))  === (0 , 0 , 0xFFFF , 0xFFFFFFFF)
      (lastAddress  $ IpBlockV6 (Ipv6Block (Ipv6Address (4, 4, 0, 0)) (Ipv6NetMask 33)))  === (4 , 0x7FFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF)

    it "should implement read" $ require $ property $ do
      read "1:2:3:4::/127"  === Ipv6Block (Ipv6Address (0x10002,0x30004,0,0)) (Ipv6NetMask 127)
      read "1234::/16"      === Ipv6Block (Ipv6Address (0x12340000, 0, 0, 0)) (Ipv6NetMask 16)
      read "12:34::/32"     === Ipv6Block (Ipv6Address (0x120034, 0, 0, 0))   (Ipv6NetMask 32)
