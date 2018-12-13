module Main where

import           Control.DeepSeq
import           Data.List
import           Weigh

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
      [ action "ip addresses to non-collapsed blocks" $ do
          ipv4bs <- fmap (\i -> I.Ipv4Block i (I.Ipv4NetMask 32)) <$> loadIpv4Addresses "data/bench/akamai.txt"
          pure $ force ipv4bs
      , action "collapsing blocks" $ do
          ipv4bs <- fmap (\i -> I.Ipv4Block i (I.Ipv4NetMask 32)) <$> loadIpv4Addresses "data/bench/akamai.txt"
          let blocks = I.collapseIpv4Blocks $ sort ipv4bs
          pure $ force blocks
      ]
  return ()
