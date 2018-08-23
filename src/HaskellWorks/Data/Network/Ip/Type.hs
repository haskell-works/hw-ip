{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HaskellWorks.Data.Network.Ip.Type
  ( Ipv4Address(..)
  , Ipv4NetMask(..)
  , Ipv4Block(..)
  ) where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Attoparsec.Text                  as AP
import qualified Data.Text                             as T
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified Text.ParserCombinators.ReadPrec       as RP

newtype Ipv4Address = Ipv4Address
  { word :: Word32
  } deriving (Enum, Eq, Ord, Generic)

instance Show Ipv4Address where
  showsPrec _ (Ipv4Address w) =
    shows ((w .>. 24) .&. 0xff) . ('.':) .
    shows ((w .>. 16) .&. 0xff) . ('.':) .
    shows ((w .>.  8) .&. 0xff) . ('.':) .
    shows ( w         .&. 0xff)

instance Read Ipv4Address where
  readsPrec :: Int -> String -> [(Ipv4Address, String)]
  readsPrec _ s = case AP.parseWith (return mempty) (I.whitespace *> I.ipv4Address) (T.pack s) of
    Just result -> case result of
      AP.Done i r   -> [(Ipv4Address r, T.unpack i)]
      AP.Partial _  -> []
      AP.Fail a b c -> []
    Nothing -> []

newtype Ipv4NetMask = Ipv4NetMask
  { word :: Word8
  } deriving (Enum, Eq, Ord, Show, Generic)

data Ipv4Block = Ipv4Block
  { base :: !Ipv4Address
  , mask :: !Ipv4NetMask
  } deriving (Eq, Ord)

instance Show Ipv4Block where
  showsPrec _ (Ipv4Block b (Ipv4NetMask m)) = shows b . ('/':) . shows m
