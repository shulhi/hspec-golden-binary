{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Binary.Internal.GoldenSpecs
-- Description : Golden tests for Arbitrary
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno-cadorette@plowtech.net
-- Stability   : Beta
--
-- Internal module, use at your own risk.
module Test.Binary.Internal.GoldenSpecs where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy hiding (putStrLn)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Proxy
import Data.Typeable
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Random
import Test.Binary.Internal.Utils
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (readFile, writeFile)
import qualified Data.Binary as Binary

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
  forall s a.
  (GoldenBinaryrConstraints s a, Typeable a, Arbitrary a, Eq a, Binary.Binary a) =>
  Settings ->
  Proxy (s a) ->
  Spec
goldenSpecs settings proxy = 
  goldenSpecsWithNote settings proxy Nothing

-- | same as 'goldenSpecs' but has the option of passing a note to the
-- 'describe' function.
goldenSpecsWithNote ::
  forall s a.
  (GoldenBinaryrConstraints s a, Typeable a, Arbitrary a, Eq a, Binary.Binary a) =>
  Settings ->
  Proxy (s a) ->
  Maybe String ->
  Spec
goldenSpecsWithNote settings proxy mNote = do
  typeNameInfo <- runIO $ mkTypeNameInfo settings (Proxy :: Proxy a)
  goldenSpecsWithNotePlain settings typeNameInfo proxy mNote

-- | same as 'goldenSpecsWithNote' but does not require a Typeable or Show instance.
goldenSpecsWithNotePlain ::
  forall s a.
  (GoldenBinaryrConstraints s a, Arbitrary a, Eq a, Binary.Binary a) =>
  Settings ->
  TypeNameInfo a ->
  Proxy (s a) ->
  Maybe String ->
  Spec
goldenSpecsWithNotePlain settings@Settings {..} typeNameInfo@(TypeNameInfo {typeNameTypeName}) proxy mNote = do
  let goldenFile = mkGoldenFile settings typeNameInfo
      note = maybe "" (" " ++) mNote
  describe ("Encoding of " ++ addBrackets (unTypeName typeNameTypeName) ++ note) $
    it ("produces the same data as is found in " ++ goldenFile) $ do
      exists <- doesFileExist goldenFile
      let fixIfFlag err = do
            doFix <- isJust <$> lookupEnv recreateBrokenGoldenEnv
            if doFix
              then createGoldenfile @s settings proxy goldenFile
              else throwIO err
      if exists
        then
          compareWithGolden @s settings proxy goldenFile comparisonFile
            `catches` [ Handler (\(err :: HUnitFailure) -> fixIfFlag err),
                        Handler (\(err :: DecodeError) -> fixIfFlag err)
                      ]
        else do
          doCreate <- isJust <$> lookupEnv createMissingGoldenEnv
          if doCreate
            then createGoldenfile @s settings proxy goldenFile
            else expectationFailure $ "Missing golden file: " <> goldenFile

-- | The golden files already exist. Binary values with the same seed from
-- the golden file and compare the with the data in the golden file.
compareWithGolden ::
  forall s a.
  (GoldenBinaryrConstraints s a, Arbitrary a, Eq a, Binary.Binary a) =>
  Settings ->
  Proxy (s a) ->
  FilePath ->
  ComparisonFile ->
  IO ()
compareWithGolden _settings _Proxy goldenFile _comparisonFile = do
  goldenBytes <- readFile goldenFile
  let (randomSamples :: RandomSamples a) = Binary.decode goldenBytes 
  Binary.encode randomSamples `shouldBe` goldenBytes

-- | The golden files do not exist. Create it.
createGoldenfile :: forall s a. (Ctx s (RandomSamples a), GoldenBinaryr s, Arbitrary a) => Settings -> Proxy (s a) -> FilePath -> IO ()
createGoldenfile Settings {..} Proxy goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO
  rSamples <- lift @s <$> mkRandomSamples sampleSize (Proxy :: Proxy a) rSeed
  writeFile goldenFile (encode rSamples)

  putStrLn $
    "\n"
      ++ "WARNING: Running for the first time, not testing anything.\n"
      ++ "  Created "
      ++ goldenFile
      ++ " containing random samples,\n"
      ++ "  will compare " ++ fileType ++ " encodings with this from now on.\n"
      ++ "  Please, consider putting "
      ++ goldenFile
      ++ " under version control."

-- | Create the file path for the golden file. Optionally use the module name to
-- help avoid name collissions. Different modules can have types of the same
-- name.
mkGoldenFile :: Settings -> TypeNameInfo a -> FilePath
mkGoldenFile Settings {..} (TypeNameInfo {typeNameTopDir, typeNameModuleName, typeNameTypeName}) =
  case typeNameModuleName of
    Nothing -> unTopDir typeNameTopDir </> unTypeName typeNameTypeName <.> fileType
    Just moduleName -> unTopDir typeNameTopDir </> unModuleName moduleName </> unTypeName typeNameTypeName <.> fileType

-- | Create a number of arbitrary instances of a type
-- a sample size and a random seed.
mkRandomSamples ::
  forall a.
  (Arbitrary a) =>
  Int ->
  Proxy a ->
  Int32 ->
  IO (RandomSamples a)
mkRandomSamples sampleSize Proxy rSeed = RandomSamples rSeed <$> generate gen
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen :: Gen [a]
    gen = setSeed (fromIntegral rSeed) $ replicateM correctedSampleSize (arbitrary :: Gen a)
