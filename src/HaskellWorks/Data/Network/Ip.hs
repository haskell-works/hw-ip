{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Network.Ip
  ( Ipv4Address(..)
  , Ipv4NetMask(..)
  , Ipv4Block(..)
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  ) where

import Data.Bits
import Data.Word

newtype Ipv4Address = Ipv4Address
  { unIpv4Address :: Word32
  } deriving (Enum, Eq, Ord)

instance Show Ipv4Address where
  showsPrec _ (Ipv4Address w) =
    shows ((w `shiftR` 24) .&. 0xff) . ('.':) .
    shows ((w `shiftR` 16) .&. 0xff) . ('.':) .
    shows ((w `shiftR`  8) .&. 0xff) . ('.':) .
    shows ( w              .&. 0xff)

newtype Ipv4NetMask = Ipv4NetMask
  { unIpv4NetMask :: Word8
  } deriving (Enum, Eq, Ord, Show)

data Ipv4Block = Ipv4Block
  { ipv4BlockBase :: Ipv4Address
  , ipv4BlockMask :: Ipv4NetMask
  } deriving (Eq, Ord)

instance Show Ipv4Block where
  showsPrec _ (Ipv4Block b (Ipv4NetMask m)) = shows b . ('/':) . shows m

bitPower :: Ipv4NetMask -> Int
bitPower (Ipv4NetMask m) = fromIntegral (32 - m)

isCanonical :: Ipv4Block -> Bool
isCanonical (Ipv4Block (Ipv4Address b) m) = ((b `shiftR` bitPower m) `shiftL` bitPower m) == b

splitBlock :: Ipv4Block -> Maybe (Ipv4Block, Ipv4Block)
splitBlock (Ipv4Block (Ipv4Address b) (Ipv4NetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = Ipv4NetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) `shiftR` fromIntegral (m + 1))
          in  Just
              ( Ipv4Block (Ipv4Address  b     ) halfMask
              , Ipv4Block (Ipv4Address (b + c)) halfMask
              )
    else  Nothing

blockSize :: Ipv4Block -> Int
blockSize (Ipv4Block _ m) = 2 ^ bitPower m
