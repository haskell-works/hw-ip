{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HaskellWorks.Data.Network.Ip.Ipv4
  ( IpAddress(..)
  , IpNetMask(..)
  , IpBlock(..)
  , bitPower
  , isCanonical
  , splitBlock
  , parseIpAddress
  , showIpAddress
  , showsIpAddress
  , tshowIpAddress
  , tshowIpBlock
  , ipAddressToWords
  , firstIpAddress
  , lastIpAddress
  , canonicaliseIpBlock
  , collapseIpBlocks
  , splitIpRange
  , rangeToBlocksDL
  , rangeToBlocks
  , blockToRange
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.Semigroup                        ((<>))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.SafeEnum
import HaskellWorks.Data.Network.Ip.Validity

import qualified Data.Bits                             as B
import qualified Data.Sequence                         as S
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified Text.Appar.String                     as AP

newtype IpAddress = IpAddress
  { word :: Word32
  } deriving (Enum, Bounded, Eq, Ord, Generic, SafeEnum)

instance Show IpAddress where
  showsPrec _ = showsIpAddress

instance Read IpAddress where
  readsPrec :: Int -> String -> [(IpAddress, String)]
  readsPrec = I.readsPrecOnParser (IpAddress <$> I.ipv4Address)

newtype IpNetMask = IpNetMask
  { word8 :: Word8
  } deriving (Enum, Bounded, Eq, Ord, Show, Generic)

-- | An IP block.  The type parameter determines whether or not the value of the type is
-- canonical.
data IpBlock v = IpBlock
  { base :: !IpAddress
  , mask :: !IpNetMask
  } deriving (Eq, Ord, Generic)

instance Show (IpBlock v) where
  showsPrec _ = showsIpBlock

instance Read (IpBlock Unaligned) where
  readsPrec = I.readsPrecOnParser parseUnalignedIpBlock

instance Read (IpBlock Canonical) where
  readsPrec = I.readsPrecOnParser parseCanonicalIpBlock

-- | Canonicalise the block by zero-ing out the host bits
canonicaliseIpBlock :: IpBlock v -> IpBlock Canonical
canonicaliseIpBlock (IpBlock (IpAddress w) (IpNetMask m)) = IpBlock (IpAddress newWord) (IpNetMask m)
  where bp = fromIntegral (32 - m)
        newWord = (w .>. bp) .<. bp

firstIpAddress :: IpBlock v -> IpAddress
firstIpAddress (IpBlock b _) = b

lastIpAddress :: IpBlock v -> IpAddress
lastIpAddress (IpBlock (IpAddress b) (IpNetMask m)) = IpAddress (b + fromIntegral (I.blockSize m) - 1)

bitPower :: IpNetMask -> Word64
bitPower (IpNetMask m) = fromIntegral (32 - m)

-- | A valid block must have all host-bits set to zero after the mask is applied
isCanonical :: IpBlock v -> Bool
isCanonical (IpBlock (IpAddress b) (IpNetMask m)) = ((b .>. I.bitPower m) .<. I.bitPower m) == b

splitBlock :: IpBlock Canonical -> Maybe (IpBlock Canonical, IpBlock Canonical)
splitBlock (IpBlock (IpAddress b) (IpNetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = IpNetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) .>. fromIntegral (m + 1))
          in  Just
              ( IpBlock (IpAddress  b     ) halfMask
              , IpBlock (IpAddress (b + c)) halfMask
              )
    else  Nothing

showsIpAddress :: IpAddress -> String -> String
showsIpAddress (IpAddress w) =
  shows ((w .>. 24) .&. 0xff) . ('.':) .
  shows ((w .>. 16) .&. 0xff) . ('.':) .
  shows ((w .>.  8) .&. 0xff) . ('.':) .
  shows ( w         .&. 0xff)

showIpAddress :: IpAddress -> String
showIpAddress ipAddress = showsIpAddress ipAddress ""

tshowIpAddress :: IpAddress -> T.Text
tshowIpAddress = T.pack . showIpAddress

showsIpBlock :: IpBlock v -> String -> String
showsIpBlock (IpBlock b (IpNetMask m)) = shows b . ('/':) . shows m

showIpBlock :: IpBlock v -> String
showIpBlock ipBlock = showsIpBlock ipBlock ""

tshowIpBlock :: IpBlock v -> T.Text
tshowIpBlock = T.pack . showIpBlock

ipAddressToWords :: IpAddress -> (Word8, Word8, Word8, Word8)
ipAddressToWords (IpAddress w) =
  ( fromIntegral (w .>. 24) .&. 0xff
  , fromIntegral (w .>. 16) .&. 0xff
  , fromIntegral (w .>.  8) .&. 0xff
  , fromIntegral (w         .&. 0xff)
  )

parseIpAddress :: AP.Parser IpAddress
parseIpAddress = IpAddress <$> I.ipv4Address

parseUnalignedIpBlock :: AP.Parser (IpBlock Unaligned)
parseUnalignedIpBlock = do
  (a, m) <- I.ipv4Block
  return (IpBlock (IpAddress a) (IpNetMask m))

parseCanonicalIpBlock :: AP.Parser (IpBlock Canonical)
parseCanonicalIpBlock = do
  b <- parseUnalignedIpBlock
  if isCanonical b
    then let IpBlock ip m = b in return (IpBlock ip m)
    else fail $ showIpBlock b <> " is not a canonical block"

splitIpRange :: Range IpAddress -> (IpBlock Canonical, Maybe (Range IpAddress))
splitIpRange (Range (IpAddress a) (IpAddress z)) = (block, remainder)
  where bpOuter   = width - B.countLeadingZeros (z + 1 - a) - 1
        bpInner   = B.countTrailingZeros ((maxBound .<. fromIntegral bpOuter) .|. a)
        block     = IpBlock (IpAddress a) (IpNetMask (fromIntegral (width - bpInner)))
        hostMask  = comp (maxBound .<. fromIntegral bpInner)
        remainder = if a + hostMask >= z
          then Nothing
          else Just (Range (IpAddress (a + hostMask + 1)) (IpAddress z))
        width = B.finiteBitSize a

-- assume distinct & sorted input
collapseIpBlocks :: [IpBlock Canonical] -> [IpBlock Canonical]
collapseIpBlocks tomerge =
  skipOverlapped $ concat $ toList <$> go S.empty tomerge
  where
    go :: S.Seq (IpBlock Canonical) -> [IpBlock Canonical] -> [S.Seq (IpBlock Canonical)]
    go m [] = [m]
    go m (b:bs) =
      case S.viewr m of
        S.EmptyR -> go (m S.|> b) bs
        m' S.:> bp -> do
          let sp@(IpBlock _ (IpNetMask msk)) = superBlock bp
          let sp'@(IpBlock _ (IpNetMask msk')) = superBlock b
          if sp == sp' then go m' (sp : bs)
          else if msk > msk' then
                 m : go (S.empty S.|> b) bs
               else
                 go (m S.|> b) bs
    superBlock (IpBlock (IpAddress w32) (IpNetMask m)) =
      IpBlock (IpAddress (w32 B..&. (0xFFFFFFFF `B.shiftL` fromIntegral (32 - (m - 1))))) (IpNetMask (m - 1))
    skipOverlapped [] = []
    skipOverlapped [b] = [b]
    skipOverlapped (b1:b2:bs) =
      if lastIpAddress b1 >= lastIpAddress b2 then
        skipOverlapped (b1:bs)
      else
        b1 : skipOverlapped (b2:bs)

rangeToBlocksDL :: Range IpAddress -> [IpBlock Canonical] -> [IpBlock Canonical]
rangeToBlocksDL r = do
  let (b, remainder) = splitIpRange r
  case remainder of
    Just rmd -> (b:) . rangeToBlocksDL rmd
    Nothing  -> (b:)

rangeToBlocks :: Range IpAddress -> [IpBlock Canonical]
rangeToBlocks r = rangeToBlocksDL r []

blockToRange :: IpBlock Canonical -> Range IpAddress
blockToRange b = uncurry Range $ bimap firstIpAddress lastIpAddress (b, b)
