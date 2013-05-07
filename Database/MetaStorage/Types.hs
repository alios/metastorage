{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}


module Database.MetaStorage.Types (MetaStorageT (..)) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Crypto.Hash
import Filesystem

import Database.MetaStorage.Classes


newtype MetaStorageT = MetaStorage FilePath
                     deriving (Show, Eq)
                              
instance MetaStorage MetaStorageT IO SHA1 where
  ms_basedir (MetaStorage fp) = fp
  
  mkMetaStorage fp = do
    createDirectory True fp

    
    return (MetaStorage fp, [])
