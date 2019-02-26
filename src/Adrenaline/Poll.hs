{-# LANGUAGE DerivingStrategies #-}

-- |
-- A poll describes how to find the status of a system.
module Adrenaline.Poll
  ( Poll (..)
  ) where

import Data.Time.Duration (Duration)

import qualified Network.Ip as Ip

-- |
-- What to do?
data Poll

  -- |
  -- Send to a system an ICMP echo request and await its ICMP echo reply. The
  -- ICMP echo reply must arrive in time and it must be well-formed.
  = IcmpEchoExchange !Ip.Address !Duration

  deriving stock (Eq, Ord, Show)
