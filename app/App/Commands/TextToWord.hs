{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.TextToWord
  ( cmdTextToWord
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Maybe                (mapMaybe)
import Options.Applicative       hiding (columns)
import Text.Read

import qualified App.Commands.Types                as Z
import qualified Data.Binary.Builder               as B
import qualified Data.ByteString.Lazy              as LBS
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IPv4
import qualified System.IO                         as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runTextToWord :: Z.TextToWordOptions -> IO ()
runTextToWord opts = do
  contents <- IO.readFile (opts ^. the @"input")
  let as  = lines contents
  let ips = mapMaybe (readMaybe @IPv4.IpAddress) as
  let ws  = fmap (^. the @"word") ips
  LBS.writeFile (opts ^. the @"output") (B.toLazyByteString (foldMap B.putWord32le ws))

optsTextToWord :: Parser Z.TextToWordOptions
optsTextToWord = Z.TextToWordOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input line seprate IP addresses in text"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Packed IPs as little-endian Word32s"
        <>  metavar "FILE"
        )

cmdTextToWord :: Mod CommandFields (IO ())
cmdTextToWord = command "text-to-word"  $ flip info idm $ runTextToWord <$> optsTextToWord
