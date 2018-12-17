{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.RangeSpec (spec) where

import Data.Either
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.Text               as AP
import qualified HaskellWorks.Data.Network.Ip.Ip    as V
import qualified HaskellWorks.Data.Network.Ip.Ipv4  as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6  as V6
import qualified HaskellWorks.Data.Network.Ip.Range as R

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.RangeSpec" $ do
  describe "Range" $ do
    it "should be tested" $ requireTest $ do
      R.mergeRanges [] === ([] :: [R.Range Int])
      R.mergeRanges [R.Range 0 5] === [R.Range 0 5]
      R.mergeRanges [R.Range 3 5, R.Range 6 7] === [R.Range 3 7]
      R.mergeRanges [R.Range 0 5, R.Range 7 7] === [R.Range 0 5, R.Range 7 7]
      R.mergeRanges [R.Range 'a' 'b', R.Range 'c' 'f'] === [R.Range 'a' 'f']
      R.mergeRanges [R.Range 'a' 'a', R.Range 'a' 'f'] === [R.Range 'a' 'f']
      R.mergeRanges [R.Range 'a' 'b', R.Range 'b' 'b'] === [R.Range 'a' 'b']
      R.mergeRanges [R.Range 'a' 'f', R.Range 'c' 'e'] === [R.Range 'a' 'f']
      R.mergeRanges [R.Range 'a' 'f', R.Range 'c' 'e', R.Range 'x' 'z'] === [R.Range 'a' 'f', R.Range 'x' 'z']

      let v6Ranges1 = [ R.Range (V6.IpAddress (0, 0, 0, 0)) (V6.IpAddress (0, 0, 0, 20))
                      , R.Range (V6.IpAddress (0, 0, 0, 21)) (V6.IpAddress (0, 0, 0, 0xffffffff))
                      , R.Range (V6.IpAddress (0, 0, 1, 0)) (V6.IpAddress (0, 0, 1, 200))
                      ]
      let v6Ranges2 = [ R.Range (V6.IpAddress (0, 0, 0, 0)) (V6.IpAddress (0, 0, 1, 200))
                      ]
      R.mergeRanges v6Ranges1 === v6Ranges2
