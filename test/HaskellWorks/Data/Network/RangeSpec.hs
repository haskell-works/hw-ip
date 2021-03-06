{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Data.Network.RangeSpec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Network.Ip.Ipv4  as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6  as V6
import qualified HaskellWorks.Data.Network.Ip.Range as R
import qualified Text.Appar.String                  as AP

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.RangeSpec" $ do
  describe "Range" $ do
    it "should be mergeable" $ requireTest $ do
      R.mergeRanges [] === ([] :: [R.Range Int])
      R.mergeRanges @Int [R.Range 0 5] === [R.Range 0 5]
      R.mergeRanges @Int [R.Range 3 5, R.Range 6 7] === [R.Range 3 7]
      R.mergeRanges @Int [R.Range 0 5, R.Range 7 7] === [R.Range 0 5, R.Range 7 7]
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

    it "should parse dash-delimited ranges" $ requireTest $ do
      AP.runParser (AP.try (R.parseRange AP.alphaNum)) "a b"   === (Nothing               , "a b")
      AP.runParser (AP.try (R.parseRange AP.alphaNum)) "a - b" === (Just (R.Range 'a' 'b'), ""   )

    it "should parse ip address ranges" $ requireTest $ do
      let r1 = [ R.Range (V4.IpAddress 0x00000000) (V4.IpAddress 0x0001ffff)
               , R.Range (V4.IpAddress 0x0000ffff) (V4.IpAddress 0x00ffffff)]
      let r2 = [ R.Range (V4.IpAddress 0x00000000) (V4.IpAddress 0x00ffffff)]
      R.mergeRanges r1 === r2
