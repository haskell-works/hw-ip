{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Network.IpSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           HaskellWorks.Network.Ip
import           Hedgehog
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.HUnit.IpSpec" $ do
  describe "Ipv4Address" $ do
    it "should implement show" $ do
      show (Ipv4Address 0x000000ff) `shouldBe` "0.0.0.255"
      show (Ipv4Address 0x0000ff00) `shouldBe` "0.0.255.0"
      show (Ipv4Address 0x00ff0000) `shouldBe` "0.255.0.0"
      show (Ipv4Address 0xff000000) `shouldBe` "255.0.0.0"
  describe "Ipv4Block" $ do
    it "should implement show" $ do
      show (Ipv4Block (Ipv4Address 0x000000ff) (Ipv4NetMask 32)) `shouldBe` "0.0.0.255/32"
      show (Ipv4Block (Ipv4Address 0x0000ff00) (Ipv4NetMask 32)) `shouldBe` "0.0.255.0/32"
      show (Ipv4Block (Ipv4Address 0x00ff0000) (Ipv4NetMask 32)) `shouldBe` "0.255.0.0/32"
      show (Ipv4Block (Ipv4Address 0xff000000) (Ipv4NetMask 32)) `shouldBe` "255.0.0.0/32"
    it "should implement splitBlock" $ do
      splitBlock ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32))) `shouldBe` Nothing
      splitBlock ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 31))) `shouldBe` Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32), Ipv4Block (Ipv4Address 0x00000001) (Ipv4NetMask 32))
      splitBlock ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 30))) `shouldBe` Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 31), Ipv4Block (Ipv4Address 0x00000002) (Ipv4NetMask 31))
      splitBlock ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  0))) `shouldBe` Just (Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  1), Ipv4Block (Ipv4Address 0x80000000) (Ipv4NetMask  1))
    it "should implement blockSize" $ do
      blockSize ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask 32))) `shouldBe` 1
      blockSize ((Ipv4Block (Ipv4Address 0x00000000) (Ipv4NetMask  0))) `shouldBe` 0x100000000

