{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.TextToWord
  ( cmdTextToWord
  ) where

import Data.Semigroup      ((<>))
import Options.Applicative hiding (columns)

import qualified App.Commands.Types as Z

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runTextToWord :: Z.TextToWordOptions -> IO ()
runTextToWord _ = return ()

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
