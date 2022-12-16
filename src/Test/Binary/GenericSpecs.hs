{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Module      : Test.Binary.GenericSpecs
-- Description : Export all necessary functions
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno.cadorette@plowtech.net
-- Stability   : Beta
--
-- This package provides tools for testing Binary serialization.
--
-- - Test that the Data.Binary instance is isomorphic.
-- - Alert you when unexpected changes in Binary serialization occur.
-- - Record binary formatting of Haskell types.
module Test.Binary.GenericSpecs 
  (
    -- * Arbitrary testing
    goldenSpecs
  , roundtripSpecs
  , roundtripAndGoldenSpecs
  , roundtripFromFile
  , roundtripFromFileWithSettings

  -- * ToADTArbitrary testing
  , goldenADTSpecs
  , roundtripADTSpecs
  , roundtripAndGoldenSpecsWithSettings
  , roundtripAndGoldenADTSpecs
  , roundtripAndGoldenADTSpecsWithSettings
  , roundtripADTFromFile
  , roundtripADTFromFileWithSettings

  -- * Make Files
  , mkGoldenFileForType

  -- * Util
  , shouldBeIdentity
  , GoldenDirectoryOption(..)
  , Settings(..)
  , defaultSettings

  -- * re-exports
  , Proxy(..)
  ) where

import Data.Proxy
import qualified Data.Binary as Binary
import Data.Typeable
import Test.Binary.Internal.ADT.GoldenSpecs (goldenADTSpecs, mkGoldenFileForType)
import Test.Binary.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)
import qualified Test.Binary.Internal.GoldenSpecs as Golden
import qualified Test.Binary.Internal.RoundtripSpecs as Roundtrip
import Test.Binary.Internal.Utils
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import qualified Test.Binary.Internal.RoundtripFromFile
import qualified Test.Binary.Internal.ADT.RoundtripFromFile

roundtripSpecs :: forall a . (Arbitrary a, Binary.Binary a, Typeable a, Show a) => Proxy a -> Spec
roundtripSpecs Proxy = Roundtrip.roundtripSpecs (Proxy :: Proxy (GoldenBinary a))

-- | Tests to ensure that the binary encoding has not unintentionally changed. This
-- could be caused by the following:
--
-- - A type's instances of the serialisation have changed.
-- - Selectors have been edited, added or deleted.
-- - You have changed version of Binary the way Binary serialization has changed
--   works.
--
-- If you run this function and the golden files do not
-- exist, it will create them for each constructor. It they do exist, it will
-- compare with golden file if it exists. Golden file encodes the serialized format of a
-- type. It is recommended that you put the golden files under revision control
-- to help monitor changes.
goldenSpecs :: 
  forall a.
  (Arbitrary a, Binary.Binary a, Typeable a, Show a, Eq a) =>
  Settings ->
  Proxy a ->
  Spec
goldenSpecs settings Proxy = Golden.goldenSpecs settings (Proxy :: Proxy (GoldenBinary a))

-- | run roundtrip and golden test for a type.
-- sampleSize is used only when creating the golden file. When it is
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenSpecs :: (Arbitrary a, Binary.Binary a, Typeable a, Show a, Eq a) => Proxy a -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

-- | 'roundtripAndGoldenSpecs' with custom settings.
roundtripAndGoldenSpecsWithSettings ::
  forall a.
  (Arbitrary a, Binary.Binary a, Typeable a, Show a, Eq a) =>
  Settings ->
  Proxy a ->
  Spec
roundtripAndGoldenSpecsWithSettings settings proxy = do
  roundtripSpecs proxy
  goldenSpecs settings proxy

-- | run roundtrip and golden tests for all constructors of a type.
-- sampleSize is used only when creating the golden files. When they are
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenADTSpecs ::
  forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, Binary.Binary a) =>
  Proxy a ->
  Spec
roundtripAndGoldenADTSpecs =
  roundtripAndGoldenADTSpecsWithSettings defaultSettings

-- | 'roundtripAndGoldenADTSpecs' with custom settings.
roundtripAndGoldenADTSpecsWithSettings ::
  forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, Binary.Binary a) =>
  Settings ->
  Proxy a ->
  Spec
roundtripAndGoldenADTSpecsWithSettings settings proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs settings proxy

newtype GoldenBinary a = GoldenBinary a deriving (Show)

instance GoldenBinaryr GoldenBinary where
  type Ctx GoldenBinary = Binary.Binary
  encode = Binary.encode . unlift
  decode = Right . lift . Binary.decode
  lift = GoldenBinary
  unlift (GoldenBinary a) = a

instance Arbitrary a => Arbitrary (GoldenBinary a) where
  arbitrary = GoldenBinary <$> arbitrary


defaultSettings :: Settings
defaultSettings = genericDefaultSettings "bin"


roundtripFromFile :: forall a. 
  (Arbitrary a, Typeable a, Eq a, Show a, Binary.Binary a)
  => Proxy a -> Spec
roundtripFromFile = roundtripFromFileWithSettings defaultSettings

roundtripFromFileWithSettings :: forall a. 
  (Arbitrary a, Typeable a, Eq a, Show a, Binary.Binary a)
  => Settings -> Proxy a -> Spec
roundtripFromFileWithSettings = Test.Binary.Internal.RoundtripFromFile.roundtripFromFile

roundtripADTFromFile :: forall a. 
  (ToADTArbitrary a, Typeable a, Eq a, Show a, Binary.Binary a)
  => Proxy a -> Spec
roundtripADTFromFile = roundtripADTFromFileWithSettings defaultSettings

roundtripADTFromFileWithSettings :: forall a. 
  (ToADTArbitrary a, Typeable a, Eq a, Show a, Binary.Binary a)
  => Settings -> Proxy a -> Spec
roundtripADTFromFileWithSettings = Test.Binary.Internal.ADT.RoundtripFromFile.roundtripADTFromFile