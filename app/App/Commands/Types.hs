{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( TextToWordOptions(..)
  ) where

import GHC.Generics

data TextToWordOptions = TextToWordOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)
