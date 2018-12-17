{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HaskellWorks.Data.Network.Ip.Ipv4
  ( IpAddress(..)
  , IpNetMask(..)
  , IpBlock(..)
  , isValidIpBlock
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  , textToMaybeIpAddress
  , parseIpAddress
  , ipAddressToString
  , ipAddressToText
  , ipAddressToWords
  , firstIpAddress
  , lastIpAddress
  , canonicaliseIpBlock
  , collapseIpBlocks
  , splitIpRange
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Network.Ip.Range
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
  } deriving (Enum, Eq, Ord, Generic)

instance Show IpAddress where
  showsPrec _ (IpAddress w) =
    shows ((w .>. 24) .&. 0xff) . ('.':) .
    shows ((w .>. 16) .&. 0xff) . ('.':) .
    shows ((w .>.  8) .&. 0xff) . ('.':) .
    shows ( w         .&. 0xff)

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
  } deriving (Enum, Eq, Ord, Show, Generic)

data IpBlock = IpBlock
  { base :: !IpAddress
  , mask :: !IpNetMask
  } deriving (Eq, Ord, Generic)

instance Show IpBlock where
  showsPrec _ (IpBlock b (IpNetMask m)) = shows b . ('/':) . shows m

instance Read IpBlock where
  readsPrec :: Int -> String -> [(IpBlock, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Block) (T.pack s) of
    Just result -> case result of
      AP.Done i (a, m) -> [(IpBlock (IpAddress a) (IpNetMask m), T.unpack i)]
      AP.Partial _     -> []
      AP.Fail a b c    -> []
    Nothing -> []

-- | A valid block must have all host-bits set to zero after the mask is applied
isValidIpBlock :: IpBlock -> Bool
isValidIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = word .<. fromIntegral mask == 0

-- | Canonicalise the block by zero-ing out the host bits
canonicaliseIpBlock :: IpBlock -> IpBlock
canonicaliseIpBlock (IpBlock (IpAddress word) (IpNetMask mask)) = IpBlock (IpAddress newWord) (IpNetMask mask)
  where bp = fromIntegral (32 - mask)
        newWord = (word .>. bp) .<. bp

firstIpAddress :: IpBlock -> IpAddress
firstIpAddress (IpBlock base _) = base

lastIpAddress :: IpBlock -> IpAddress
lastIpAddress b@(IpBlock (IpAddress base) _) = IpAddress (base + fromIntegral (blockSize b) - 1)

bitPower :: IpNetMask -> Word64
bitPower (IpNetMask m) = fromIntegral (32 - m)

isCanonical :: IpBlock -> Bool
isCanonical (IpBlock (IpAddress b) m) = ((b .>. bitPower m) .<. bitPower m) == b

splitBlock :: IpBlock -> Maybe (IpBlock, IpBlock)
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

blockSize :: IpBlock -> Int
blockSize (IpBlock _ m) = 2 ^ bitPower m

textToMaybeIpAddress :: T.Text -> Maybe IpAddress
textToMaybeIpAddress t = AP.maybeResult =<< AP.parseWith (return mempty) parseIpAddress t

ipAddressToString :: IpAddress -> String
ipAddressToString = show

ipAddressToText :: IpAddress -> T.Text
ipAddressToText = T.pack . ipAddressToString

ipAddressToWords :: IpAddress -> (Word8, Word8, Word8, Word8)
ipAddressToWords (IpAddress w) =
  ( fromIntegral (w .>. 24) .&. 0xff
  , fromIntegral (w .>. 16) .&. 0xff
  , fromIntegral (w .>.  8) .&. 0xff
  , fromIntegral (w         .&. 0xff)
  )

parseIpAddress :: AP.Parser IpAddress
parseIpAddress = IpAddress <$> I.ipv4Address

splitIpRange :: Range IpAddress -> (IpBlock, Maybe (Range IpAddress))
splitIpRange (Range (IpAddress a) (IpAddress z)) = (block, remainder)
  where bpOuter   = 32 - DB.countLeadingZeros (z + 1 - a) - 1
        bpInner   = DB.countTrailingZeros ((0xffffffff .<. fromIntegral bpOuter) .|. a)
        block     = IpBlock (IpAddress a) (IpNetMask (32 - fromIntegral bpInner))
        hostMask  = comp (0xffffffff .<. fromIntegral bpInner) :: Word32
        remainder = if a + hostMask >= z
          then Nothing
          else Just (Range (IpAddress (a + hostMask + 1)) (IpAddress z))

-- assume distinct & sorted input
collapseIpBlocks :: [IpBlock] -> [IpBlock]
collapseIpBlocks tomerge =
  skipOverlapped $ concat $ toList <$> go S.empty tomerge
  where
    go :: S.Seq IpBlock -> [IpBlock] -> [S.Seq IpBlock]
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

rangeToBlocksDL :: Range IpAddress -> [IpBlock] -> [IpBlock]
rangeToBlocksDL = error "TODO implement rangeToBlocksDL"

rangeToBlocks :: Range IpAddress -> [IpBlock]
rangeToBlocks = error "TODO implement rangeToBlocks"

blockToRange :: IpBlock -> IpAddress
blockToRange = error "TODO implement blockToRange"
