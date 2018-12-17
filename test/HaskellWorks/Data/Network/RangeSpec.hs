{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.RangeSpec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Network.Ip.Ip   as V
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.RangeSpec" $ do
  describe "Range" $ do
    it "should be tested" $ requireTest $ do
      True === True
