module Adrenaline.System.Gen
  ( systemId
  ) where

import Adrenaline.System (SystemId (..))
import Test.QuickCheck.Gen (Gen)

import qualified Data.Uuid.Gen as Gen

{-# INLINE systemId #-}
systemId :: Gen SystemId
systemId = SystemId <$> Gen.uuid
