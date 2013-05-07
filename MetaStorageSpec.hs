module Main (main) where

import Test.Hspec
import Database.MetaStorage.Spec
  
main :: IO ()
main = hspec $ metaStorageSpec 
