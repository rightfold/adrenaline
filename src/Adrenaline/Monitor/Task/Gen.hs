module Adrenaline.Monitor.Task.Gen
  ( task
  ) where

import Adrenaline.Monitor.Task (Task (..))
import Test.QuickCheck.Gen (Gen)

import qualified Adrenaline.Poll.Gen as Gen
import qualified Adrenaline.System.Gen as Gen

{-# INLINE task #-}
task :: Gen Task
task = Task <$> Gen.systemId <*> Gen.poll
