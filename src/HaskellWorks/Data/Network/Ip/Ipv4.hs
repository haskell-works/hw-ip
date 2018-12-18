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
  , isValidIpBlock
  , isCanonical
  , splitBlock
  , textToMaybeIpAddress
  , parseIpAddress
  , showIpAddress
  , showsIpAddress
  , tshowIpAddress
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

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Semigroup                        ((<>))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.Validity
import Text.Read

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.Bits                             as B
import qualified Data.Bits                             as DB
import qualified Data.Sequence                         as S
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype IpAddress = IpAddress
  { word :: Word32
  } deriving (Enum, Bounded, Eq, Ord, Generic)

instance Show IpAddress where
  showsPrec _ = showsIpAddress

instance Read IpAddress where
  readsPrec :: Int -> String -> [(IpAddress, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Address) (T.pack s) of
    Just result -> case result of
      AP.Done i r   -> [(IpAddress r, T.unpack i)]
      AP.Partial _  -> []
      AP.Fail a b c -> []
    Nothing -> []

newtype IpNetMask = IpNetMask
  { word8 :: Word8
  } deriving (Enum, Bounded, Eq, Ord, Show, Generic)

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

-- | A valid block must have all host-bits set to zero after the mask is applied
isValidIpBlock :: IpBlock v -> Bool
isValidIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = word .<. fromIntegral mask == 0

-- | Canonicalise the block by zero-ing out the host bits
canonicaliseIpBlock :: IpBlock v -> IpBlock Canonical
canonicaliseIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = IpBlock (IpAddress newWord) (IpNetMask mask)
  where bp = fromIntegral (32 - mask)
        newWord = (word .>. bp) .<. bp

firstIpAddress :: IpBlock v -> IpAddress
firstIpAddress (IpBlock base _) = base

lastIpAddress :: IpBlock v -> IpAddress
lastIpAddress b@(IpBlock (IpAddress base) (IpNetMask m)) = IpAddress (base + fromIntegral (I.blockSize m) - 1)

bitPower :: IpNetMask -> Word64
bitPower (IpNetMask m) = fromIntegral (32 - m)

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

textToMaybeIpAddress :: T.Text -> Maybe IpAddress
textToMaybeIpAddress t = AP.maybeResult =<< AP.parseWith (return mempty) parseIpAddress t

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
  where bpOuter   = 32 - DB.countLeadingZeros (z + 1 - a) - 1
        bpInner   = DB.countTrailingZeros ((0xffffffff .<. fromIntegral bpOuter) .|. a)
        block     = IpBlock (IpAddress a) (IpNetMask (32 - fromIntegral bpInner))
        hostMask  = comp (0xffffffff .<. fromIntegral bpInner) :: Word32
        remainder = if a + hostMask >= z
          then Nothing
          else Just (Range (IpAddress (a + hostMask + 1)) (IpAddress z))

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
          let sp@(IpBlock _ (IpNetMask mask)) = superBlock bp
          let sp'@(IpBlock _ (IpNetMask mask')) = superBlock b
          if sp == sp' then go m' (sp : bs)
          else if mask > mask' then
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
    Just rem -> (b:) . rangeToBlocksDL rem
    Nothing  -> (b:)

rangeToBlocks :: Range IpAddress -> [IpBlock Canonical]
rangeToBlocks r = rangeToBlocksDL r []

blockToRange :: IpBlock Canonical -> Range IpAddress
blockToRange b = uncurry Range $ bimap firstIpAddress lastIpAddress (b, b)
