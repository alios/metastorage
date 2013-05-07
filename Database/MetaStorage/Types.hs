{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}


module Database.MetaStorage.Types (MetaStorageT (..), mkMetaStorageDefault) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import Crypto.Hash
import Filesystem
import Control.Monad.IO.Class
import Database.MetaStorage.Classes


newtype MetaStorageT = MetaStorage FilePath
                     deriving (Show, Eq)
                              
instance (MonadIO m, HashAlgorithm h) => MetaStorage MetaStorageT m h where
  ms_basedir (MetaStorage fp) = fp
  
  mkMetaStorage fp = liftIO $  do
    createDirectory True fp

    
    return (MetaStorage fp, [])


mkMetaStorageDefault :: (MonadIO m) => FilePath -> m (MetaStorageT, [Digest SHA1])
mkMetaStorageDefault = mkMetaStorage
