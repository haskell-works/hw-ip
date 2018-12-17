{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.Data.Network.Ip.SafeEnum where

class SafeEnum a where
  safePred :: a -> Maybe a
  safeSucc :: a -> Maybe a

  unsafePred :: a -> a
  unsafeSucc :: a -> a

instance (Bounded a, Enum a, Eq a) => SafeEnum a where
  safePred v = if v /= minBound then Just (pred v) else Nothing
  safeSucc v = if v /= maxBound then Just (succ v) else Nothing
  unsafePred = pred
  unsafeSucc = succ
