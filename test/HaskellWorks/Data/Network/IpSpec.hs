module HaskellWorks.Data.Network.IpSpec
where

import HaskellWorks.Data.Network.Ip.Ip
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                    as Gen
import Hedgehog.Range                  as Range
import Test.Hspec

import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.IpSpec" $ do
  describe "IpAddress" $ do
    it "Should show and read ip v6" $ require $ property $ do
      a <- forAll $ Gen.word32 Range.constantBounded
      b <- forAll $ Gen.word32 Range.constantBounded
      c <- forAll $ Gen.word32 Range.constantBounded
      d <- forAll $ Gen.word32 Range.constantBounded

      let addr = IpAddressV6 (V6.IpAddress (a, b, c, d))
      read (show addr) === addr

  describe "IpAddress" $ do
    it "Should show and read ip v4" $ require $ property $ do
      a <- forAll $ Gen.word32 Range.constantBounded

      let addr = IpAddressV4 (V4.IpAddress a)
      read (show addr) === addr
