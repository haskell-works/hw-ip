module HaskellWorks.Data.Network.IpSpec
where

import HaskellWorks.Data.Network.Ip.Ip
import HaskellWorks.Data.Network.Unsafe
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6
import qualified Hedgehog.Gen                      as G
import qualified Hedgehog.Range                    as R

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Network.IpSpec" $ do
  describe "IpAddress" $ do
    it "Should show and read ip v6" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      b <- forAll $ G.word32 R.constantBounded
      c <- forAll $ G.word32 R.constantBounded
      d <- forAll $ G.word32 R.constantBounded

      let addr = IpAddressV6 (V6.IpAddress (a, b, c, d))
      read (unsafeShow addr) === addr

  describe "IpAddress" $ do
    it "Should show and read ip v4" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded

      let addr = IpAddressV4 (V4.IpAddress a)
      read (unsafeShow addr) === addr
