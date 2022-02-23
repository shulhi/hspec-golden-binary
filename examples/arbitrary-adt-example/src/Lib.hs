{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( Person (..),
    OnOrOff (..),
  )
where

-- base

-- Binary
import Data.Binary
import GHC.Generics (Generic)
-- QuickCheck
import Test.QuickCheck (Arbitrary (..), oneof)
-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data Person = Person
  { name :: String,
    address :: String,
    age :: Int
  }
  deriving (Eq, Read, Show, Generic)

instance Binary Person

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

data OnOrOff
  = On
  | Off
  deriving (Eq, Read, Show, Generic)

instance Binary OnOrOff

instance ToADTArbitrary OnOrOff

instance Arbitrary OnOrOff where
  arbitrary = oneof [pure On, pure Off]
