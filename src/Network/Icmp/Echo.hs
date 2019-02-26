{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Currently, ICMP echo exchanges are implemented by invoking the ping program.
-- This is easier than implementing it ourselves, but eventually we want to,
-- because that is more efficient.
module Network.Icmp.Echo
  ( PingException (..)
  , ping
  , pingCommand
  ) where

import Control.Exception (Exception, SomeException, throwIO)
import Control.Exception.Sync (catchSync)
import Control.Lens ((^.), to)
import Data.AffineSpace ((.-.))
import Data.Time.Duration (Duration)
import Text.Printf (printf)

import qualified Data.Time.Instant as Instant
import qualified Data.Time.Duration as Duration
import qualified Network.Ip as Ip
import qualified System.Process as Process

-- |
-- Ping failed.
newtype PingException =
  PingException SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

-- |
-- Execute a ping command.
ping :: Ip.Address -> Duration -> IO Duration
ping a w = do
  let (cp, ca) = pingCommand a w
  before <- Instant.now
  Process.callProcess cp ca
    `catchSync` (throwIO . PingException)
  after <- Instant.now
  pure $ after .-. before

-- |
-- Ping command for an ICMP echo exchange.
pingCommand :: Ip.Address -> Duration -> (String, [String])
pingCommand a w = ("/usr/bin/env", ["ping", "-c", "1", "-W", w', "--", a'])
  where w' = w ^. Duration.seconds . to pingTimeout . to (printf "%d")
        a' = Ip.formatAddress a

-- |
-- Patch a double so that it can be used as the timeout argument to ping.
{-# INLINE pingTimeout #-}
pingTimeout :: Double -> Int
pingTimeout =
  -- Ping has two quirks that we need to work around: it only accepts integer
  -- timeouts, and the timeout must not be zero. If the timeout is zero, it is
  -- interpreted as no timeout instead.
  max 1 . round
