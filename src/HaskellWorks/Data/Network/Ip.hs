{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Data.Network.Ip
  ( Z.Ipv4Address(Ipv4Address)
  , Z.Ipv4NetMask(Ipv4NetMask)
  , Z.Ipv4Block(Ipv4Block)
  , bitPower
  , blockSize
  , isCanonical
  , splitBlock
  , textToMaybeIpv4Address
  , ipv4AddressesToBlocks
  , ipv4AddressToString
  , ipv4AddressToText
  , ipv4AddressToWords
  , firstIpv4Address
  , lastIpv4Address
  , buildIpv4AddressTree
  , collapseIpv4Blocks
  , collapseIpv4Blocks'
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Attoparsec.Text                     as AP
import qualified Data.Bits                                as B
import qualified Data.HashTable.Class                     as H
import qualified Data.HashTable.ST.Cuckoo                 as HC
import qualified Data.List                                as DL
import qualified Data.Map.Strict                          as M
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

ipv4AddressesToBlocks :: [Z.Ipv4Address] -> [Z.Ipv4Block]
ipv4AddressesToBlocks ips = do
  let tree = buildIpv4AddressTree ips
  go tree 0 0 []
  where
    go :: IPTree -> Word8 -> Word32 -> [Z.Ipv4Block] -> [Z.Ipv4Block]
    go Leaf _ _ = id
    go root@(Node left right) level base =
      if findLargestCompleteTree root then
        ((Z.Ipv4Block (Z.Ipv4Address base) (Z.Ipv4NetMask level)) :)
      else do
        let rightBase = 0x80000000 `B.shiftR` fromIntegral level .|. base
        go left (level + 1) base . go right (level + 1) rightBase

data IPTree = Leaf | Node IPTree IPTree deriving (Show, Eq, Generic, NFData)

addIpv4AddressToTree :: IPTree -> Z.Ipv4Address -> IPTree
addIpv4AddressToTree root ipv4@(Z.Ipv4Address w) =
  go root 0 w
    where
      go :: IPTree -> Int -> Word32 -> IPTree
      go Leaf level w = go (Node Leaf Leaf) level w
      go (Node left right) level w =
        if level < 32 then
          if B.testBit w (fromIntegral $ 31 - level) then
            Node left (go right (level + 1) w)
          else
            Node (go left (level + 1) w) right
        else
          Node Leaf Leaf

buildIpv4AddressTree :: [Z.Ipv4Address] -> IPTree
buildIpv4AddressTree = foldl addIpv4AddressToTree Leaf

findLargestCompleteTree :: IPTree -> Bool
findLargestCompleteTree Leaf                   = True
findLargestCompleteTree (Node Leaf (Node _ _)) = False
findLargestCompleteTree (Node (Node _ _) Leaf) = False
findLargestCompleteTree (Node l r)             = findLargestCompleteTree l && findLargestCompleteTree r

maxHeight :: IPTree -> Int
maxHeight Leaf              = 0
maxHeight (Node left right) = 1 + max (maxHeight left) (maxHeight right)

collapseIpv4Blocks :: [Z.Ipv4Block] -> [Z.Ipv4Block]
collapseIpv4Blocks tomerge =
  skipOverlapped . DL.sort $ go M.empty tomerge
  where
    go :: M.Map Word32 Z.Ipv4Block -> [Z.Ipv4Block] -> [Z.Ipv4Block]
    go m [] = snd <$> M.toList m
    go m (b:bs) =
      let superB@(Z.Ipv4Block (Z.Ipv4Address key) _) = superBlock b
      in case m M.!? key of
        Nothing         -> go (M.insert key b m) bs
        Just x | x /= b -> go (M.delete key m) (superB : bs)
        _               -> go m bs
    superBlock (Z.Ipv4Block (Z.Ipv4Address w32) (Z.Ipv4NetMask m)) =
      Z.Ipv4Block (Z.Ipv4Address (w32 B..&. (0xFFFFFFFF `B.shiftL` fromIntegral (32 - (m - 1))))) (Z.Ipv4NetMask (m - 1))
    skipOverlapped [] = []
    skipOverlapped [b] = [b]
    skipOverlapped (b1:b2:bs) =
      if lastIpv4Address b1 >= lastIpv4Address b2 then
        skipOverlapped (b1:bs)
      else
        b1 : skipOverlapped (b2:bs)

type HashTable s k v = HC.HashTable s k v
collapseIpv4Blocks' :: [Z.Ipv4Block] -> [Z.Ipv4Block]
collapseIpv4Blocks' tomerge = do
  let results = runST $ do
                  subnets <- H.new
                  go subnets tomerge
                  H.toList subnets

  skipOverlapped . DL.sort $ snd <$> results
  where
    go :: HashTable s Word32 Z.Ipv4Block -> [Z.Ipv4Block] -> ST s ()
    go _ [] = return ()
    go h (b:bs) = do
      let superB@(Z.Ipv4Block (Z.Ipv4Address hkey) _) = superBlock b
      exists <- H.lookup h hkey
      case exists of
        Nothing         -> do
          H.insert h hkey b
          go h bs
        Just x | x /= b -> do
          H.delete h hkey
          go h (superB : bs)
        _               -> go h bs
    superBlock (Z.Ipv4Block (Z.Ipv4Address w32) (Z.Ipv4NetMask m)) =
      Z.Ipv4Block (Z.Ipv4Address (w32 B..&. (0xFFFFFFFF `B.shiftL` fromIntegral (32 - (m - 1))))) (Z.Ipv4NetMask (m - 1))
    skipOverlapped [] = []
    skipOverlapped [b] = [b]
    skipOverlapped (b1:b2:bs) =
      if lastIpv4Address b1 >= lastIpv4Address b2 then
        skipOverlapped (b1:bs)
      else
        b1 : skipOverlapped (b2:bs)
