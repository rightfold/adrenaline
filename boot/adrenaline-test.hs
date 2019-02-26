module Main
  ( main
  ) where

import Test.Hspec (hspec)

import qualified Adrenaline.Monitor.Scheduler.Loop.Spec

main :: IO ()
main = hspec $
  Adrenaline.Monitor.Scheduler.Loop.Spec.spec
