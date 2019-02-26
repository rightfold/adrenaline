module Adrenaline.Monitor.Scheduler.Loop.Gen
  ( tick
  ) where

import Adrenaline.Monitor.Scheduler.Loop (Inbound (..))
import Test.QuickCheck.Gen (Gen)

import qualified Data.Time.Instant.Gen as Gen

{-# INLINE tick #-}
tick :: Gen Inbound
tick = Tick <$> Gen.instant
