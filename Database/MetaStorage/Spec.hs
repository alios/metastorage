{-# LANGUAGE OverloadedStrings #-}

module Database.MetaStorage.Spec (metaStorageSpec) where

import Test.Hspec
import Test.QuickCheck
import Filesystem.Path.CurrentOS
import Crypto.Hash
import Filesystem

import Database.MetaStorage
import Database.MetaStorage.Types

metaStorageSpec :: Spec
metaStorageSpec = do
  describe "MetaStorage.mkMetaStorage" $ do
    it "creates a new storage in FilePath" $ example $ do
      (st, _) <- (mkMetaStorage fp)
      st `shouldBe` (MetaStorage fp) 
    it "throws an exception on invalid filepath" $ example $ do
      let ms = (mkMetaStorage invalidPath) :: IO (MetaStorageT, [Digest SHA1])
      ms `shouldThrow` anyException
    context "when given an non existing dir" $ do
      it "the new directory will be created" $ example $ do
        removeTree fp
        (_, ds) <- (mkMetaStorage fp) :: IO (MetaStorageT, [Digest SHA1])
        isDir <- isDirectory fp
        isDir `shouldBe` True

      it "returns an empty list " $ example $ do
        removeTree fp
        (_, ds) <- (mkMetaStorage fp) :: IO (MetaStorageT, [Digest SHA1])
        ds `shouldBe` []
        
    it "returns a list of the hashes of existing items" $ example $ do
      pendingWith "no tests implemented yet"

  describe "MetaStorage.ms_basedir" $ do
    it "returns the basedir of the MetaStorage" $ do
      (st, _) <- (mkMetaStorage fp) :: IO (MetaStorageT, [Digest SHA1])
      ms_basedir st `shouldBe` fp 
    
  describe "MetaStorage.ms_getFilePath" $ do
    it "returns an absolute FilePath for a given digest" $ 
      pendingWith "unimplemented test"

  where fpbase = "/tmp"
        fp = fpbase </> fromText "mstesttmp"
        invalidPath = empty

