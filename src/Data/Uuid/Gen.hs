module Data.Uuid.Gen
  ( uuid
  ) where

import Data.UUID.Types (UUID)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen)

import qualified Data.UUID.Types as Uuid

uuid :: Gen UUID
uuid = Uuid.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
