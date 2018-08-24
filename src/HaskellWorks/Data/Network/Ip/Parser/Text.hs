module HaskellWorks.Data.Network.Ip.Parser.Text
  ( ipv4Address
  ) where

import qualified Data.Attoparsec.Text                  as AP
import qualified HaskellWorks.Data.Network.Ip.Internal as I
import qualified HaskellWorks.Data.Network.Ip.Type     as HW

ipv4Address :: AP.Parser HW.Ipv4Address
ipv4Address = HW.Ipv4Address <$> I.ipv4Address
