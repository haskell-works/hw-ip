module Main where

import Control.DeepSeq
import Weigh

import qualified HaskellWorks.Data.Network.Ip as I
import qualified System.IO                    as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

loadIpv4Addresses :: FilePath -> IO [I.Ipv4Address]
loadIpv4Addresses filePath = fmap read . lines <$> IO.readFile filePath

main :: IO ()
main = do
  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    sequence_
      [ action "ip addresses to binary tree" $ do
          ipv4s <- loadIpv4Addresses "data/bench/akamai.txt"
          let tree = I.buildIpv4AddressTree ipv4s
          pure $ force tree
      , action "collapsing ip addresses to blocks (binary tree)" $ do
          ipv4s <- loadIpv4Addresses "data/bench/akamai.txt"
          let blocks = I.ipv4AddressesToBlocks ipv4s
          pure $ force blocks
      , action "ip addresses to non-collapsed blocks" $ do
          ipv4bs <- fmap (\i -> I.Ipv4Block i (I.Ipv4NetMask 32)) <$> loadIpv4Addresses "data/bench/akamai.txt"
          pure $ force ipv4bs
      , action "collapsing blocks (map)" $ do
          ipv4bs <- fmap (\i -> I.Ipv4Block i (I.Ipv4NetMask 32)) <$> loadIpv4Addresses "data/bench/akamai.txt"
          let blocks = I.collapseIpv4Blocks ipv4bs
          pure $ force blocks
      , action "collapsing blocks (mutable hashtable)" $ do
          ipv4bs <- fmap (\i -> I.Ipv4Block i (I.Ipv4NetMask 32)) <$> loadIpv4Addresses "data/bench/akamai.txt"
          let blocks = I.collapseIpv4Blocks' ipv4bs
          pure $ force blocks
      ]
  return ()
