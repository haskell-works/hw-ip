module HaskellWorks.Data.Network.Ip where

import HaskellWorks.Data.Network.Ip.Internal as X
import HaskellWorks.Data.Network.Ip.Ip       as X
import HaskellWorks.Data.Network.Ip.Ipv4     as X
import HaskellWorks.Data.Network.Ip.Ipv6     as X
import HaskellWorks.Data.Network.Ip.Type     as X

import Data.Foldable
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Attoparsec.Text                     as AP
import qualified Data.Bits                                as B
import qualified Data.Sequence                            as S
import qualified Data.Text                                as T
import qualified HaskellWorks.Data.Network.Ip.Internal    as I
import qualified HaskellWorks.Data.Network.Ip.Parser.Text as APT
import qualified HaskellWorks.Data.Network.Ip.Type        as Z

bitPower :: Z.Ipv4NetMask -> Word64
bitPower (Z.Ipv4NetMask m) = fromIntegral (32 - m)

isCanonical :: Z.Ipv4Block -> Bool
isCanonical (Z.Ipv4Block (Z.Ipv4Address b) m) = ((b .>. bitPower m) .<. bitPower m) == b

splitBlock :: Z.Ipv4Block -> Maybe (Z.Ipv4Block, Z.Ipv4Block)
splitBlock (Z.Ipv4Block (Z.Ipv4Address b) (Z.Ipv4NetMask m)) =
  if m >= 0 && m < 32
    then  let !hm       = m + 1
              !halfMask = Z.Ipv4NetMask hm
              !c        = fromIntegral ((0x100000000 :: Word64) .>. fromIntegral (m + 1))
          in  Just
              ( Z.Ipv4Block (Z.Ipv4Address  b     ) halfMask
              , Z.Ipv4Block (Z.Ipv4Address (b + c)) halfMask
              )
    else  Nothing

blockSize :: Z.Ipv4Block -> Int
blockSize (Z.Ipv4Block _ m) = 2 ^ bitPower m

firstIpv4Address :: Z.Ipv4Block -> Z.Ipv4Address
firstIpv4Address (Z.Ipv4Block base _) = base

lastIpv4Address :: Z.Ipv4Block -> Z.Ipv4Address
lastIpv4Address b@(Z.Ipv4Block (Z.Ipv4Address base) _) = Z.Ipv4Address (base + fromIntegral (blockSize b) - 1)

textToMaybeIpv4Address :: T.Text -> Maybe Z.Ipv4Address
textToMaybeIpv4Address t = AP.maybeResult =<< AP.parseWith (return mempty) APT.ipv4Address t

ipv4AddressToString :: Z.Ipv4Address -> String
ipv4AddressToString = show

ipv4AddressToText :: Z.Ipv4Address -> T.Text
ipv4AddressToText = T.pack . ipv4AddressToString

ipv4AddressToWords :: Z.Ipv4Address -> (Word8, Word8, Word8, Word8)
ipv4AddressToWords (Z.Ipv4Address w) =
  ( fromIntegral (w .>. 24) .&. 0xff
  , fromIntegral (w .>. 16) .&. 0xff
  , fromIntegral (w .>.  8) .&. 0xff
  , fromIntegral (w         .&. 0xff)
  )

-- assume distinct & sorted input
collapseIpv4Blocks :: [Z.Ipv4Block] -> [Z.Ipv4Block]
collapseIpv4Blocks tomerge =
  skipOverlapped $ concat $ toList <$> go S.empty tomerge
  where
    go :: S.Seq Z.Ipv4Block -> [Z.Ipv4Block] -> [S.Seq Z.Ipv4Block]
    go m [] = [m]
    go m (b:bs) =
      case S.viewr m of
        S.EmptyR -> go (m S.|> b) bs
        m' S.:> bp -> do
          let sp@(Z.Ipv4Block _ (Z.Ipv4NetMask mask)) = superBlock bp
          let sp'@(Z.Ipv4Block _ (Z.Ipv4NetMask mask')) = superBlock b
          if sp == sp' then go m' (sp : bs)
          else if mask > mask' then
                 m : go (S.empty S.|> b) bs
               else
                 go (m S.|> b) bs
    superBlock (Z.Ipv4Block (Z.Ipv4Address w32) (Z.Ipv4NetMask m)) =
      Z.Ipv4Block (Z.Ipv4Address (w32 B..&. (0xFFFFFFFF `B.shiftL` fromIntegral (32 - (m - 1))))) (Z.Ipv4NetMask (m - 1))
    skipOverlapped [] = []
    skipOverlapped [b] = [b]
    skipOverlapped (b1:b2:bs) =
      if lastIpv4Address b1 >= lastIpv4Address b2 then
        skipOverlapped (b1:bs)
      else
        b1 : skipOverlapped (b2:bs)
