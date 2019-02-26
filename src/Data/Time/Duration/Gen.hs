module Data.Time.Duration.Gen
  ( duration
  ) where

import Data.Time.Duration (Duration (..))
import Test.QuickCheck.Gen (Gen)

import qualified Test.QuickCheck.Gen as Gen

{-# INLINE duration #-}
duration :: Gen Duration
duration = Duration <$> Gen.choose (-1e6, 1e6)
