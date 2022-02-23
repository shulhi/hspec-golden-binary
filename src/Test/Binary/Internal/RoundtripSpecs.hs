{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Binary.Internal.RoundtripSpecs
-- Description : Roundtrip tests for Arbitrary
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno.cadorette@plowtech.net
-- Stability   : Beta
--
-- Internal module, use at your own risk.
module Test.Binary.Internal.RoundtripSpecs where

import Data.Typeable
import Test.Binary.Internal.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | A roundtrip test to check whether values of the given type
-- can be successfully converted to binary and back to a Haskell value.
--
-- 'roundtripSpecs' will
--
-- - create random values (using 'Arbitrary'),
-- - convert them into binary (using 'Data.Binary.put'),
-- - read them back into Haskell (using 'Data.Binary.get') and
-- - make sure that the result is the same as the value it started with
--   (using 'Eq').
roundtripSpecs ::
  forall s a.
  (Typeable a, Show (s a), Arbitrary (s a), GoldenBinaryr s, Ctx s a) =>
  Proxy (s a) ->
  Spec
roundtripSpecs proxy = genericBinaryRoundtripWithNote proxy Nothing

-- | Same as 'roundtripSpecs', but optionally add notes to the 'describe'
-- function.
genericBinaryRoundtripWithNote ::
  forall s a.
  (Typeable a, Show (s a), Arbitrary (s a), GoldenBinaryr s, Ctx s a) =>
  Proxy (s a) ->
  Maybe String ->
  Spec
genericBinaryRoundtripWithNote proxy mNote = do
  let typeIdentifier = show (typeRep (Proxy :: Proxy a))
  genericBinaryRoundtripWithNotePlain proxy mNote typeIdentifier

-- | Same as 'genericBinaryRoundtripWithNote', but no need for Typeable, Eq, or Show
genericBinaryRoundtripWithNotePlain ::
  forall s a.
  (Show (s a), Arbitrary (s a), GoldenBinaryr s, Ctx s a) =>
  Proxy (s a) ->
  Maybe String ->
  String ->
  Spec
genericBinaryRoundtripWithNotePlain _ mNote typeIdentifier = do
  let note = maybe "" (" " ++) mNote

  describe ("Binary encoding of " ++ addBrackets typeIdentifier ++ note) $
    prop
      "allows to encode values with binary and read them back"
      (checkEncodingEquality @s @a)
