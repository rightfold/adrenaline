{-# LANGUAGE DerivingStrategies #-}

module Adrenaline.Monitor.Task
  ( Task (..)
  , systemId
  , poll
  ) where

import Adrenaline.Poll (Poll)
import Adrenaline.System (SystemId)
import Control.Lens (Lens', lens)

-- |
-- A task identifies a poll.
data Task =
  Task
    { taskSystemId :: !SystemId
    , taskPoll     :: !Poll }
  deriving stock (Eq, Ord, Show)

-- |
-- Optic for 'taskSystemId'.
systemId :: Lens' Task SystemId
systemId = lens taskSystemId (\s a -> s { taskSystemId = a })

-- |
-- Optic for 'taskPoll'.
poll :: Lens' Task Poll
poll = lens taskPoll (\s a -> s { taskPoll = a })
