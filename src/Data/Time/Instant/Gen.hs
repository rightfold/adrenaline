module Data.Time.Instant.Gen
  ( instant
  ) where

import Data.Time.Instant (Instant (..))
import Test.QuickCheck.Gen (Gen)

import qualified Test.QuickCheck.Gen as Gen

{-# INLINE instant #-}
instant :: Gen Instant
instant = Instant <$> Gen.choose (-1e6, 1e6)
