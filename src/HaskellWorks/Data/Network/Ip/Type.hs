{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module HaskellWorks.Data.Network.Ip.Type
  ( I4.Ipv4Address(..)
  , I4.Ipv4NetMask(..)
  , I4.Ipv4Block(..)
  , I6.Ipv6Address(..)
  , I6.Ipv6NetMask(..)
  , I6.Ipv6Block(..)
  ) where

import qualified HaskellWorks.Data.Network.Ip.Ipv4 as I4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as I6
