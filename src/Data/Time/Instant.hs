{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Time.Instant
  ( Instant (..)
  , now
  ) where

import Data.AffineSpace (AffineSpace (..))
import Data.Time.Duration (Duration (..))

import qualified System.Posix.Time as Unix

-- |
-- An instant is a point on a universal non-relativistic timeline. Use an
-- instant to tell when an event happens, and to tell whether one event happens
-- before or after another event.
--
-- Not to be confused with a point on a calendar. In particular, you cannot get
-- a date from an instant without extra information such as a time zone and a
-- leap second database.
newtype Instant =
  Instant Double
  deriving stock (Eq, Ord, Show)

instance AffineSpace Instant where
  type Diff Instant = Duration
  (.-.) (Instant a) (Instant b) = Duration (a - b)
  (.+^) (Instant a) (Duration b) = Instant (a + b)

-- |
-- Get the current time.
now :: IO Instant
now = Instant . realToFrac <$> Unix.epochTime
