module App.Commands where

import App.Commands.RangeStats
import App.Commands.TextToWord
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdTextToWord
  <>  cmdRangeStats
