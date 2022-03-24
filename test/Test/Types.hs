{-# LANGUAGE DeriveGeneric #-}

module Test.Types where

import Data.Binary
import Data.Binary.Get
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

instance Binary Person

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary

data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 Double String Int
  deriving (Eq, Show, Generic)

instance Binary SumType

instance ToADTArbitrary SumType

instance Arbitrary SumType where
  arbitrary = genericArbitrary

newtype ConsumeAll = ConsumeAll [Int] deriving (Eq, Show, Generic)

instance Binary ConsumeAll where
  get = fmap (ConsumeAll . decode) getRemainingLazyByteString
  put (ConsumeAll xs) = put xs

instance ToADTArbitrary ConsumeAll

instance Arbitrary ConsumeAll where
  arbitrary = genericArbitrary

