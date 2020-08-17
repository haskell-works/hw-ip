{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( TextToWordOptions(..)
  , RangeStatsOptions(..)
  ) where

import GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4  as IPv4
import qualified HaskellWorks.Data.Network.Ip.Range as R

data TextToWordOptions = TextToWordOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)

newtype RangeStatsOptions = RangeStatsOptions
  { range  :: R.Range IPv4.IpAddress
  } deriving (Eq, Show, Generic)
