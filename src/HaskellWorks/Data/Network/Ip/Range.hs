{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HaskellWorks.Data.Network.Ip.Range where

import Data.List                             (unfoldr)
import GHC.Generics
import HaskellWorks.Data.Network.Ip.SafeEnum
import Prelude                               hiding (last)

import qualified Text.Appar.String as AP

data Range a = Range
  { first :: a
  , last  :: a
  } deriving (Eq, Ord, Show, Generic)

parseRange :: AP.Parser a -> AP.Parser (Range a)
parseRange pa = Range <$> pa <* AP.string " - " <*> pa

-- | Merge adjacent ranges if they overlap or are adjacent
mergeRanges :: (SafeEnum a, Ord a) => [Range a] -> [Range a]
mergeRanges (r1@(Range f1 l1):r2@(Range f2 l2):rs)
  | boundedSucc l1 >= f2  = mergeRanges (nr:rs)
  | otherwise = r1 : mergeRanges (r2:rs)
  where nr = Range f1 (max l1 l2)
mergeRanges [r] = [r]
mergeRanges [] = []

class Contains a where
  -- | 'left' contains 'right', with the possibility of one or both of the boundaries being the same.
  contains :: a -> a -> Bool

instance Ord a => Contains (Range a) where
  contains l r = first l <= first r && last l >= last r

rangeToList :: (SafeEnum a, Ord a) => Range a -> [a]
rangeToList (Range a b) = takeWhile (<= b) $ unfoldr (\x -> (x,) <$> safeSucc x) a
