{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaskellWorks.Data.Network.Ip.Word128 where

import Data.Maybe
import Data.Word
import Prelude    hiding (words)

import qualified Data.Bits as B

type Word128 = (Word32, Word32, Word32, Word32)

instance Enum Word128 where
  fromEnum w = fromIntegral . word128ToInteger $ w
  toEnum i   = integerToWord128 $ fromIntegral i
  succ (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff) = (0, 0, 0, 0)
  succ (a,          0xffffffff, 0xffffffff, 0xffffffff) = (succ a, 0, 0, 0)
  succ (a,                   b, 0xffffffff, 0xffffffff) = (a, succ b, 0, 0)
  succ (a,                   b,          c, 0xffffffff) = (a, b, succ c, 0)
  succ (a,                   b,          c,          d) = (a, b, c, succ d)
  pred (0, 0, 0, 0) = (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)
  pred (a, 0, 0, 0) = (    pred a, 0xffffffff, 0xffffffff, 0xffffffff)
  pred (a, b, 0, 0) = (         a,     pred b, 0xffffffff, 0xffffffff)
  pred (a, b, c, 0) = (         a,          b,     pred c, 0xffffffff)
  pred (a, b, c, d) = (         a,          b,          c,     pred d)

integerToWord128 :: Integer -> Word128
integerToWord128 i = let a  = fromIntegral (i `B.shiftR` 96 B..&. 0xffffffff)
                         b  = fromIntegral (i `B.shiftR` 64 B..&. 0xffffffff)
                         c  = fromIntegral (i `B.shiftR` 32 B..&. 0xffffffff)
                         d  = fromIntegral (i `B.shiftR` 00 B..&. 0xffffffff)
                     in (a, b, c, d)

word128ToInteger :: Word128 -> Integer
word128ToInteger (a, b, c, d) = let a' = fromIntegral a `B.shift` 96
                                    b' = fromIntegral b `B.shift` 64
                                    c' = fromIntegral c `B.shift` 32
                                    d' = fromIntegral d `B.shift` 0
                                in a' B..|. b' B..|. c' B..|. d' :: Integer

instance Num Word128 where
  (+) l r     = integerToWord128 $ (word128ToInteger l) + (word128ToInteger r)
  (-) l r     = integerToWord128 $ (word128ToInteger l) - (word128ToInteger r)
  (*)         = undefined
  abs a       = a
  signum (0, 0, 0, 0) = minBound
  signum _            = 1
  fromInteger = integerToWord128

instance B.Bits Word128 where
  (.&.) (a, b, c, d) (e, f, g, h) = (a B..&. e, b B..&. f, c B..&. g, d B..&. h)
  (.|.) (a, b, c, d) (e, f, g, h) = (a B..|. e, b B..|. f, c B..|. g, d B..|. h)
  xor (a, b, c, d) (e, f, g, h)   = (a `B.xor` e, b `B.xor` f, c `B.xor` g, d `B.xor` h)
  complement (a, b, c, d)         = (B.complement a, B.complement b, B.complement c, B.complement d)
  shift w n                       = integerToWord128 $ (word128ToInteger w) `B.shift` n
  shiftL w n
    | n < 0 = minBound  -- This is the special case to make it behaviour as the same as Word32
    | otherwise = integerToWord128 $ (word128ToInteger w) `B.shiftL` n
  shiftR w n                      = integerToWord128 $ (word128ToInteger w) `B.shiftR` n
  rotate w n                      = integerToWord128 $ (word128ToInteger w) `B.rotate` n
  rotateL w n                     = integerToWord128 $ (word128ToInteger w) `B.rotateL` n
  rotateR w n                     = integerToWord128 $ (word128ToInteger w) `B.rotateR` n
  bitSize _                       = 128
  bitSizeMaybe _                  = Just 128
  isSigned _                      = False
  testBit w n                     = B.testBit (word128ToInteger w) n
  bit n                           = integerToWord128 $ B.bit n
  popCount w                      = B.popCount $ word128ToInteger w

instance B.FiniteBits Word128 where
  finiteBitSize _ = 128
