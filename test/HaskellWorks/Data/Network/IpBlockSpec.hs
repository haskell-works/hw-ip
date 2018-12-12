{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.IpBlockSpec (spec) where

import HaskellWorks.Data.Network.Ip
import HaskellWorks.Data.Network.Ip.Block
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
  describe "IpBlock" $ do
    it "should implement show" $ requireTest $ do
      show (IpBlockV6 (Ipv6Block (Ipv6Address (3, 3, 3, 0)) (Ipv6NetMask 96))) === "0:3:0:3:0:3::/96"

    it "should implement read" $ requireTest $ do
      read "1:2:3:4::/127"  === IpBlockV6 (Ipv6Block (Ipv6Address (0x10002,0x30004,0,0)) (Ipv6NetMask 127))
      read "1234::/16"      === IpBlockV6 (Ipv6Block (Ipv6Address (0x12340000, 0, 0, 0)) (Ipv6NetMask  16))
      read "12:34::/32"     === IpBlockV6 (Ipv6Block (Ipv6Address (0x120034, 0, 0, 0))   (Ipv6NetMask  32))
