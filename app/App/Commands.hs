module App.Commands where

import App.Commands.RangeStats
import App.Commands.TextToWord
import Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdTextToWord
  <>  cmdRangeStats
