{-# LANGUAGE DerivingStrategies #-}

module Adrenaline.System
  ( SystemId (..)
  ) where

import Data.UUID.Types (UUID)

-- |
-- Which system are we talking about?
newtype SystemId =
  SystemId UUID
  deriving stock (Eq, Ord, Show)
