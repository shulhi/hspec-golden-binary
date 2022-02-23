{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Binary.Internal.ADT.RoundtripSpecs
-- Description : Roundtrip tests for ToADTArbitrary
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno-cadorette@plowtech.net
-- Stability   : Beta
--
-- Internal module, use at your own risk.
module Test.Binary.Internal.ADT.RoundtripSpecs where
  
import Data.Foldable (traverse_)
import qualified Data.Binary as B
import Data.Typeable
import Test.Binary.Internal.Utils
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

-- | A roundtrip test to check whether values of all of constructors of the
-- given type can be successfully converted to binary and back to a Haskell value.
--
-- 'roundtripADTSpecs' will
--
-- - create random values for each constructor using 'ToADTArbitrary',
-- - convert them into binary using 'Data.Binary.get',
-- - read them back into Haskell using 'Data.Binary.put' and
-- - make sure that the result is the same as the value it started with
--   using 'Eq'.
roundtripADTSpecs ::
  forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, B.Binary a) =>
  Proxy a ->
  Spec
roundtripADTSpecs proxy = genericBinaryRoundtripADTWithNote proxy Nothing

-- | Same as 'roundtripADTSpecs' but has the option of passing a note to the
-- 'describe' function.
genericBinaryRoundtripADTWithNote ::
  forall a.
  (ToADTArbitrary a, Eq a, Show a, Arbitrary a, B.Binary a) =>
  Proxy a ->
  Maybe String ->
  Spec
genericBinaryRoundtripADTWithNote _ mNote = do
  adt <- runIO $ generate (toADTArbitrary (Proxy :: Proxy a))
  describe ("Binary encoding of " ++ addBrackets (adtTypeName adt) ++ note) $
    it "allows to encode values with binary and read them back" $
      traverse_ (serializeRoundtrip . capArbitrary) $ adtCAPs adt
  where
    note = maybe "" (" " ++) mNote

serializeRoundtrip :: (Eq a, B.Binary a, Show a) => a -> Expectation
serializeRoundtrip a = B.decode (B.encode a) `shouldBe` a
