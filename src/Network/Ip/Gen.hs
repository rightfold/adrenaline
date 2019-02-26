module Network.Ip.Gen
  ( address
  , addressV4
  , addressV6
  ) where

import Network.Ip (Address (..))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen)

import qualified Test.QuickCheck.Gen as Gen

{-# INLINE address #-}
address :: Gen Address
address = Gen.oneof [addressV4, addressV6]

{-# INLINE addressV4 #-}
addressV4 :: Gen Address
addressV4 = AddressV4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

{-# INLINE addressV6 #-}
addressV6 :: Gen Address
addressV6 = AddressV6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
