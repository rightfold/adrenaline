{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Adrenaline.Monitor.Scheduler.Loop
  ( -- * Commands
    Inbound (..)
  , Outbound (..)

    -- * Loop
  , Loop
  , loop
  , stepAwait
  , stepTick
  , stepSchedule
  , stepCancel
  ) where

import Adrenaline.Monitor.Scheduler.State (State)
import Adrenaline.Monitor.Task (Task)
import Adrenaline.System (SystemId)
import Control.Lens ((^.), to)
import Control.Monad.Trans.State (StateT)
import Data.Foldable (traverse_)
import Data.Time.Instant (Instant)
import Pipes (Pipe)

import qualified Adrenaline.Monitor.Scheduler.State as State
import qualified Adrenaline.Monitor.Task as Task
import qualified Pipes

--------------------------------------------------------------------------------
-- Commands

-- |
-- An inbound command is sent to a loop.
data Inbound
  = Tick !Instant
  | Schedule !Instant !Task
  | Cancel !SystemId
  deriving stock (Eq, Ord, Show)

-- |
-- An outbound command is emitted from a loop.
data Outbound
  = Execute !Task
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Loop

-- |
-- A loop may receive and emit events, and update its state. It cannot have any
-- other effects and cannot return.
type Loop f = forall a. Pipe Inbound Outbound (StateT State f) a

-- |
-- Start a loop.
loop :: Monad f => Loop f
loop = stepAwait

stepAwait :: Monad f => Loop f
stepAwait =
  Pipes.await >>= \case
    Tick n -> stepTick n
    Schedule i t -> stepSchedule i t
    Cancel s -> stepCancel s

stepTick :: Monad f => Instant -> Loop f
stepTick n = do
  ts <- State.dequeue n
  traverse_ (Pipes.yield . Execute . snd) ts
  stepAwait

stepSchedule :: Monad f => Instant -> Task -> Loop f
stepSchedule i t = do
  State.schedule i t
  stepAwait

stepCancel :: Monad f => SystemId -> Loop f
stepCancel s = do
  State.cancel (^. Task.systemId . to (== s))
  stepAwait
