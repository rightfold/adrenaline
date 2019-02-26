-- |
-- Rarely do you want to catch asynchronous exceptions. The functions in this
-- module will automatically rethrow asynchronous exceptions for you.
module Control.Exception.Sync
  ( catchSync
  ) where

import Control.Exception (Exception, Handler (..), SomeAsyncException, catches, throwIO)

-- |
-- Like 'catch', but rethrow async exceptions.
catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync a h = a `catches` [Handler h', Handler h]
  where h' = throwIO :: SomeAsyncException -> IO a
