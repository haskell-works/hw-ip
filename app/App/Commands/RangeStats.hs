{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.RangeStats
  ( cmdRangeStats
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types                 as Z
import qualified HaskellWorks.Data.Network.Ip.Ipv4  as IPv4
import qualified HaskellWorks.Data.Network.Ip.Range as R
import qualified Text.Appar.String                  as AP

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runRangeStats :: Z.RangeStatsOptions -> IO ()
runRangeStats opts =
  print $ length . R.rangeToList $ opts ^. the @"range"

optsRangeStats :: Parser Z.RangeStatsOptions
optsRangeStats = Z.RangeStatsOptions
  <$> option (maybeReader parseRange)
        (   long "range"
        <>  short 'r'
        <>  help "IP Range"
        <>  metavar "RANGE"
        )

parseRange :: String -> Maybe (R.Range IPv4.IpAddress)
parseRange = AP.parse (R.parseRange IPv4.parseIpAddress)

cmdRangeStats :: Mod CommandFields (IO ())
cmdRangeStats = command "range-stats"  $ flip info idm $ runRangeStats <$> optsRangeStats
