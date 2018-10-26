module Main where

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
      [ action "collapse to blocks" $ do
          ipv4s <- loadIpv4Addresses "data/bench/akamai.txt"
          pure $ I.ipv4AddressesToBlocks ipv4s
      ]
  return ()
