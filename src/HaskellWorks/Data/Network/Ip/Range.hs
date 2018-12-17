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
mergeRanges (r1@(Range f1 l1):r2@(Range f2 l2):rs) =
  if succ l1 >= f2
    then
    let nr = Range f1 (max l1 l2)
    in mergeRanges (nr:rs)
  else r1 : mergeRanges (r2:rs)
mergeRanges [r] = [r]
mergeRanges [] = []
