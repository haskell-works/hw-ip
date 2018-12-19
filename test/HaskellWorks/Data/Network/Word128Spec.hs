{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Word128Spec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Bits                            as B
import qualified HaskellWorks.Data.Network.Ip.Word128 as W

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.Ipv6Spec" $ do
  describe "Word128" $ do
    it "should implement +" $ requireTest $ do
      ((1, 1, 1, 1) :: W.Word128) + ((1, 1, 1, 1) :: W.Word128) === ((2, 2, 2, 2) :: W.Word128)
      ((1, 1, 1, 0xffffffff) :: W.Word128) + ((1, 1, 1, 1) :: W.Word128) === ((2, 2, 3, 0) :: W.Word128)
      ((0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff) :: W.Word128) + ((0, 0, 0, 1) :: W.Word128) === ((0, 0, 0, 0) :: W.Word128)

    it "should implement +" $ requireTest $ do
      ((1, 1, 1, 1) :: W.Word128) - ((1, 1, 1, 1) :: W.Word128) === ((0, 0, 0, 0) :: W.Word128)
      ((1, 1, 1, 0xffffffff) :: W.Word128) - ((1, 1, 1, 1) :: W.Word128) === ((0, 0, 0, 0xfffffffe) :: W.Word128)
      ((0, 0, 0, 1) :: W.Word128) - ((0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff) :: W.Word128) === ((0, 0, 0, 2) :: W.Word128)

    it "should implement shift" $ requireTest $ do
      ((1, 1, 1, 1) :: W.Word128) `B.shift` (16 :: Int) === ((0x10000, 0x10000, 0x10000, 0x10000) :: W.Word128)
      ((1, 1, 1, 1) :: W.Word128) `B.shift` (32 :: Int) === ((1, 1, 1, 0) :: W.Word128)
      ((1, 1, 1, 0xffffffff) :: W.Word128) `B.shift` (32 :: Int) === ((1, 1, 0xffffffff, 0) :: W.Word128)
      ((1, 1, 1, 1) :: W.Word128) `B.shiftR` (16 :: Int) === ((0, 0x10000, 0x10000, 0x10000) :: W.Word128)
      ((1, 1, 1, 1) :: W.Word128) `B.shiftR` (32 :: Int) === ((0, 1, 1, 1) :: W.Word128)
      ((1, 1, 0xffffffff, 1) :: W.Word128) `B.shiftR` (32 :: Int) === ((0, 1, 1, 0xffffffff) :: W.Word128)
      (maxBound :: W.Word128) `B.shift` (1 :: Int) === ((0xffffffff, 0xffffffff, 0xffffffff, 0xfffffffe) :: W.Word128)

    it "should implement Bits" $ requireTest $ do
      B.countLeadingZeros ((0, 0, 0, 0) :: W.Word128) === 128
      B.countLeadingZeros ((0, 0, 0, 1) :: W.Word128) === 127
      B.countLeadingZeros ((1, 1, 1, 1) :: W.Word128) === 31
      B.countTrailingZeros ((0, 0, 0, 1) :: W.Word128) === 0
      B.countTrailingZeros ((0, 0, 0, 0) :: W.Word128) === 128
      B.countTrailingZeros ((0, 0, 0x10, 0) :: W.Word128) === 36
