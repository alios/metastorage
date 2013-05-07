{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Database.MetaStorage.Classes (MetaStorage (..)) where

import Prelude hiding (FilePath)
import qualified Data.ByteString as BS
import Data.Char (chr)
import qualified Data.Serialize as Ser
import Data.Conduit
import Data.Conduit.Filesystem
import Data.Conduit.Cereal
import Control.Monad.IO.Class
import Crypto.Hash
import Filesystem.Path.CurrentOS


class (MonadIO m) => MetaStorage st m d | st -> d, st -> m where
  mkMetaStorage :: FilePath -> m (st, [Digest d])
  ms_basedir :: st -> FilePath 
  ms_getFilePath :: st -> Digest d -> FilePath
  
  ms_import :: (Ser.Serialize t) => st -> Sink t m (Digest d)
  ms_lookup :: (Ser.Serialize t) => st -> Digest d -> t

  ms_getFilePath st d =
    let dbs = digestToHexByteString d
        (ds', fn') = BS.splitAt (BS.length dbs `div` 2) dbs 
        bs2str bs = map (chr . fromInteger . toInteger) $ BS.unpack bs
        fn = decodeString $ bs2str fn'
        ds = foldl (</>) (ms_basedir st) [decodeString [d'] | d' <- bs2str ds']
    in ds </> fn
  
  ms_import st = do
    i' <- await
    case (i') of
      Just i -> undefined

