{-# LANGUAGE DerivingStrategies #-}

-- |
-- Internet Protocol.
module Network.Ip
  ( -- * Addresses
    Address (..)
  , formatAddress
  ) where

import Data.Word (Word8, Word16)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Addresses

-- |
-- Address.
data Address
  = AddressV4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
              {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
  | AddressV6 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
              {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
              {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
              {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
  deriving stock (Eq, Ord, Show)

-- |
-- Format an address.
{-# INLINE formatAddress #-}
formatAddress :: Address -> String
formatAddress (AddressV4 a b c d) = printf "%d.%d.%d.%d" a b c d
formatAddress (AddressV6 a b c d e f g h) = printf "%x:%x:%x:%x:%x:%x:%x:%x"
                                                   a b c d e f g h
