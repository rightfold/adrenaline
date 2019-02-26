{-# LANGUAGE ParallelListComp #-}

module Adrenaline.Monitor.Scheduler.Loop.Spec
  ( spec
  ) where

import Adrenaline.Monitor.Scheduler.Loop (Inbound (..), Outbound (..), loop)
import Adrenaline.Monitor.Scheduler.State (State)
import Control.Monad.Trans.State (runState)
import Data.List (sort, splitAt)
import Pipes ((>->))
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)

import qualified Adrenaline.Monitor.Scheduler.Loop.Gen as Gen
import qualified Adrenaline.Monitor.Scheduler.State as State
import qualified Adrenaline.Monitor.Task.Gen as Gen
import qualified Data.Time.Instant.Gen as Gen
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import qualified Test.QuickCheck.Gen as Gen

spec :: Spec
spec =
  describe "Adrenaline.Monitor.Scheduler.Loop" $ do

    let runLoop :: Foldable f => f Inbound -> ([Outbound], State)
        runLoop inbounds =
          let
            s = State.empty
            p = Pipes.each inbounds >-> loop
          in
            runState (Pipes.toListM p) s

    describe "Tick" $ do

      prop "does nothing if the queue is empty" $
        forAll (Gen.listOf Gen.tick) $ \ts ->
          let
            (o, s') = runLoop ts
          in
            do { o  `shouldBe` []
               ; s' `shouldBe` State.empty }

      prop "emits no commands if all tasks are scheduled in the future" $
        forAll (Gen.listOf Gen.task   ) $ \as ->
        forAll (Gen.listOf Gen.instant) $ \is ->
          let
            (tis, sis) = splitHalf (sort is)
            ss = [Schedule i a | i <- sis | a <- as]
            ts = [Tick i       | i <- tis          ]
            (o, _) = runLoop (ss <> ts)
          in
            o `shouldBe` []

      prop "emits execute commands for tasks scheduled in the past" $
        forAll (Gen.listOf Gen.task   ) $ \as  ->
        forAll (Gen.listOf Gen.instant) $ \sis ->
        forAll             Gen.instant  $ \ti  ->
          let
            ss = [Schedule i a | i <- sis | a <- as]
            ts = [Tick ti]
            (o, _) = runLoop (ss <> ts)
          in
            sort o `shouldBe`
              sort [Execute u | Schedule t u <- ss, t <= ti]

splitHalf :: [a] -> ([a], [a])
splitHalf = splitAt =<< (`div` 2) . length
