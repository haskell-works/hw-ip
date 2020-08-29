module HaskellWorks.Data.Network.Unsafe
  ( unsafeShow
  ) where

{- HLINT ignore "Avoid restricted function" -}

unsafeShow :: Show a => a -> String
unsafeShow = show
