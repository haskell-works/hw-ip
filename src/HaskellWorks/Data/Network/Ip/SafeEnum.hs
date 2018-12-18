module HaskellWorks.Data.Network.Ip.SafeEnum
  ( SafeEnum(..)
  , boundedPred
  , boundedSucc
  ) where

import Data.Int
import Data.Maybe
import Data.Word

class SafeEnum a where
  safePred :: a -> Maybe a
  safeSucc :: a -> Maybe a

instance SafeEnum Word where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Word8 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Word16 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Word32 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Word64 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Int where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Int8 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Int16 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Int32 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Int64 where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Char where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Bool where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum () where
  safePred = defaultSafePred
  safeSucc = defaultSafeSucc

instance SafeEnum Integer where
  safePred = Just . pred
  safeSucc = Just . succ

boundedPred :: SafeEnum a => a -> a
boundedPred a = fromMaybe a (safePred a)

boundedSucc :: SafeEnum a => a -> a
boundedSucc a = fromMaybe a (safeSucc a)

defaultSafePred :: (Bounded a, Enum a, Eq a) => a -> Maybe a
defaultSafePred v = if v /= minBound then Just (pred v) else Nothing

defaultSafeSucc :: (Bounded a, Enum a, Eq a) => a -> Maybe a
defaultSafeSucc v = if v /= minBound then Just (succ v) else Nothing
