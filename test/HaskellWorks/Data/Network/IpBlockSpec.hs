{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Data.Network.IpBlockSpec (spec) where

import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Network.Ip.Ip   as V
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.IpSpec" $ do
  describe "IpBlock" $ do
    it "should implement show" $ requireTest $ do
      show (V.IpBlockV6 @Unaligned (V6.IpBlock (V6.IpAddress (3, 3, 3, 0)) (V6.IpNetMask 96))) === "0:3:0:3:0:3::/96"

    it "should implement read" $ requireTest $ do
      read "1:2:3:4::/127"  === V.IpBlockV6 @Unaligned (V6.IpBlock (V6.IpAddress (0x10002    , 0x30004 , 0, 0)) (V6.IpNetMask 127))
      read "1234::/16"      === V.IpBlockV6 @Unaligned (V6.IpBlock (V6.IpAddress (0x12340000 , 0       , 0, 0)) (V6.IpNetMask  16))
      read "12:34::/32"     === V.IpBlockV6 @Unaligned (V6.IpBlock (V6.IpAddress (0x120034   , 0       , 0, 0)) (V6.IpNetMask  32))
