{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Network.Ip.Range where

import GHC.Generics

import qualified Data.Attoparsec.Text as AP

data Range a = Range
  { first :: a
  , last  :: a
  } deriving (Eq, Ord, Show, Generic)

parseRange :: Ord a => AP.Parser a -> AP.Parser (Range a)
parseRange pa = Range <$> pa <* AP.string " - " <*> pa

-- | Merge adjacent ranges if they overlap or are adjacent
mergeRanges :: (Enum a, Ord a) => [Range a] -> [Range a]
mergeRanges = error "TODO implement mergeRanges"
