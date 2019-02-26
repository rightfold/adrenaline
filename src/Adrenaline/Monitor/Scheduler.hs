{-# LANGUAGE NumericUnderscores #-}

-- |
-- A scheduler is a process that keeps track of tasks and ensures they are
-- executed in a timely fashion. A scheduler does not execute any tasks itself;
-- it emits them.
module Adrenaline.Monitor.Scheduler
  ( -- * High-level
    Scheduler
  , start
  , stop

    -- ** Commands
  , schedule
  , cancel

    -- * Low-level
  , loopM
  , clock
  ) where

import Adrenaline.Monitor.Task (Task)
import Adrenaline.Monitor.Scheduler.Loop (Inbound (..), Outbound (..), loop)
import Adrenaline.System (SystemId)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT)
import Data.Time.Instant (Instant)
import Data.Void (Void)
import Pipes (Consumer, Producer, (>->))

import qualified Adrenaline.Monitor.Scheduler.State as State
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan.Unagi as Chan
import qualified Data.Time.Instant as Instant
import qualified Pipes

--------------------------------------------------------------------------------
-- High-level

-- |
-- Handle to a running scheduler.
data Scheduler =
  Scheduler
    { schedulerSend  :: Inbound -> IO ()
    , schedulerLoop  :: Async Void
    , schedulerClock :: Async Void }

-- |
-- Start a scheduler.
start :: (Task -> IO ()) -> IO Scheduler
start k = do
  (ii, oi) <- Chan.newChan

  loopA  <- Async.async $ loopM (Chan.readChan oi) onOutbound
  clockA <- Async.async $ clock (Chan.writeChan ii)

  pure $ Scheduler (Chan.writeChan ii) loopA clockA

  where

  onOutbound :: Outbound -> IO ()
  onOutbound (Execute t) = k t

-- |
-- Stop a scheduler.
stop :: Scheduler -> IO ()
stop m = do
  Async.cancel (schedulerLoop  m)
  Async.cancel (schedulerClock m)

----------------------------------------
-- Commands

-- |
-- Schedule a task to be executed after a certain instant.
schedule :: Scheduler -> Instant -> Task -> IO ()
schedule m i t = schedulerSend m $ Schedule i t

-- |
-- Cancel all scheduler task for a certain system.
cancel :: Scheduler -> SystemId -> IO ()
cancel m s = schedulerSend m $ Cancel s

--------------------------------------------------------------------------------
-- Low-level

-- |
-- Like 'loop', but with actions instead of pipes.
loopM :: Monad f => f Inbound -> (Outbound -> f ()) -> f a
loopM i o =
  let e = actionProducer (lift i) >-> loop >-> kleisliConsumer (lift . o) in
  evalStateT (Pipes.runEffect e) State.empty

-- |
-- Send 'Tick' commands periodically.
clock :: (Inbound -> IO ()) -> IO a
clock k = forever $ do
  threadDelay 1_000_000
  now <- Instant.now
  k (Tick now)

actionProducer :: Monad f => f a -> Producer a f b
actionProducer a = forever $ lift a >>= Pipes.yield

kleisliConsumer :: Monad f => (a -> f ()) -> Consumer a f b
kleisliConsumer k = forever $ Pipes.await >>= lift . k
