{-|
Module      : Test.Binary.Internal.RoundtripFromFile
Description : Golden tests for Arbitrary
Copyright   : (c) Plow Technologies, 2022
License     : BSD3
Maintainer  : bruno.cadorette@plowtech.net
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Binary.Internal.RoundtripFromFile where

import           Control.Exception
import           Data.ByteString.Lazy hiding (putStrLn)
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Typeable

import           Prelude hiding (readFile, writeFile)

import           System.Directory
import           System.Environment (lookupEnv)
import           System.FilePath
import           System.Random

import           Test.Binary.Internal.Utils
import           Test.Hspec
import           Test.HUnit.Lang (HUnitFailure)
import           Test.QuickCheck
import qualified Test.Binary.Internal.GoldenSpecs as Golden
import qualified Data.Binary as Binary


roundtripFromFile :: forall a.
  (Typeable a, Arbitrary a, Eq a, Show a, Binary.Binary a)
  => Settings -> Proxy a -> Spec
roundtripFromFile settings proxy = do
  runIO $ putStrLn "testttttttttttt"
  typeNameInfo <- runIO $ mkTypeNameInfo settings proxy
  let goldenFile = Golden.mkGoldenFile settings typeNameInfo

  createMissing <- runIO $ isJust <$> lookupEnv "CREATE_MISSING_GOLDEN"
  let fixIfFlag err = do
        doFix <- isJust <$> lookupEnv "RECREATE_BROKEN_GOLDEN"
        if doFix then do
          createGoldenfile settings proxy goldenFile
        else
          throwIO err
  describe ("Encoding of " ++ addBrackets  (unTypeName $ typeNameTypeName typeNameInfo)) $
    it ("produces the same data as is found in " ++ goldenFile) $ do

      fileExist <- doesFileExist goldenFile
      if fileExist then do
        putStrLn "lol le fichier existe deja"
        runTestFromFile proxy goldenFile
          `catches`
            [
              Handler (\(err :: HUnitFailure) -> fixIfFlag err),
              Handler (\(err :: DecodeError) -> fixIfFlag err)
            ]
      else if createMissing then do
        putStrLn "create missing"
        createGoldenfile settings proxy goldenFile
      else do
        putStrLn "missing file ça arrivera pas"
        expectationFailure $ "Missing golden file: " ++ goldenFile

runTestFromFile :: forall a . (Eq a, Show a, Binary.Binary a) => Proxy a -> String -> IO ()
runTestFromFile Proxy goldenFile = do
  bytes <- readFile goldenFile
  print $ Data.ByteString.Lazy.length bytes
  let (randomSamples :: RandomSamples a) = Binary.decode bytes 
  Binary.encode randomSamples `shouldBe` bytes
  
createGoldenfile :: forall a. (Binary.Binary a, Arbitrary a) => Settings -> Proxy a -> FilePath -> IO ()
createGoldenfile Settings {..} Proxy goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO
  (rSamples :: RandomSamples a) <- Golden.mkRandomSamples sampleSize (Proxy :: Proxy a) rSeed
  let encoding = Binary.encode rSamples
  print $ Data.ByteString.Lazy.length encoding
  writeFile goldenFile encoding