module Adrenaline.Poll.Gen
  ( poll
  ) where

import Adrenaline.Poll (Poll (..))
import Test.QuickCheck.Gen (Gen)

import qualified Data.Time.Duration.Gen as Gen
import qualified Network.Ip.Gen as Gen

{-# INLINE poll #-}
poll :: Gen Poll
poll = IcmpEchoExchange <$> Gen.address <*> Gen.duration
