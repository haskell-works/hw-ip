{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Ipv6Spec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec


import qualified Data.Attoparsec.Text              as AP
import qualified Data.Text                         as T
import qualified HaskellWorks.Data.Network.Ip.Ip   as V
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R
import qualified Text.Read                         as TR

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
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
      read "1:2:3:4::/127"  === V6.IpBlock (V6.IpAddress (0x10002   , 0x30004 , 0, 0)) (V6.IpNetMask 127)
      read "1234::/16"      === V6.IpBlock (V6.IpAddress (0x12340000, 0       , 0, 0)) (V6.IpNetMask  16)
      read "12:34::/32"     === V6.IpBlock (V6.IpAddress (0x120034  , 0       , 0, 0)) (V6.IpNetMask  32)

    it "should parse what it has shown" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      m <- forAll $ G.word8 $ R.linear 0 128
      let addr = V6.IpBlock (V6.IpAddress (a, 0, 0, 0)) (V6.IpNetMask 1)
      V6.parseIpBlock (T.pack (show addr)) === Right addr
