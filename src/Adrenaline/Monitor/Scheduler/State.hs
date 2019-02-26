{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Adrenaline.Monitor.Scheduler.State
  ( -- * State
    State (..)
  , empty
  , force

    -- * Queue
  , Queue
  , queue
  , schedule
  , dequeue
  , cancel
  ) where

import Adrenaline.Monitor.Task (Task)
import Control.Lens (Lens', (%=), (%%=), lens)
import Control.Monad.State.Class (MonadState)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Time.Instant (Instant)

import qualified Control.Monad.State.Class as State
import qualified Data.PQueue.Prio.Min as MinPQueue

--------------------------------------------------------------------------------
-- State

-- |
-- Monitor state.
data State =
  State
    { stateQueue :: !Queue }
  deriving stock (Eq, Show)

{-# INLINE empty #-}
empty :: State
empty = State MinPQueue.empty

-- |
-- Force evaluation of the state before continuing.
{-# INLINE force #-}
force :: MonadState s f => f ()
force = State.modify' id

--------------------------------------------------------------------------------
-- Queue

-- |
-- Tasks and when to execute them.
type Queue =
  MinPQueue Instant Task

-- |
-- Optic for 'stateQueue'.
{-# INLINE queue #-}
queue :: Lens' State Queue
queue = lens stateQueue (\s a -> s { stateQueue = a })

-- |
-- Insert a task into an appropriate place in the queue.
{-# INLINE schedule #-}
schedule :: MonadState State f => Instant -> Task -> f ()
schedule i t = queue %= MinPQueue.insert i t

-- |
-- Find the tasks that are ready and remove them from the queue.
{-# INLINE dequeue #-}
dequeue :: MonadState State f => Instant -> f [(Instant, Task)]
dequeue n = queue %%= MinPQueue.spanWithKey (const . (<= n))

-- |
-- Cancel all tasks for which the given predicate returns true.
{-# INLINE cancel #-}
cancel :: MonadState State f => (Task -> Bool) -> f ()
cancel p = queue %= MinPQueue.filter (not . p)
