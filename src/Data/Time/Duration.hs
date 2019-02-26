{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Time.Duration
  ( Duration (..)
  , seconds
  ) where

import Control.Lens (Iso', iso)
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.Monoid (Sum (..))
import Data.VectorSpace (VectorSpace (..))

-- |
-- An duration is a difference between two instants. Use a duration to tell how
-- long an event takes.
--
-- Not to be confused with a difference between two points on a calendar. For
-- example, you cannot get a number of months from a duration.
newtype Duration =
  Duration Double
  deriving stock (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via Sum Double

instance AdditiveGroup Duration where
  zeroV = Duration 0.0
  (^+^) (Duration a) (Duration b) = Duration (a + b)
  negateV (Duration a) = (Duration (- a))

instance VectorSpace Duration where
  type Scalar Duration = Double
  (*^) a (Duration b) = Duration (a * b)

-- |
-- Optic for seconds.
{-# INLINE seconds #-}
seconds :: Iso' Duration Double
seconds = iso (\case Duration a -> a) Duration
