{-# LANGUAGE FlexibleContexts #-}
module Data.Dictionary( Dictionary (..)
                      , readDictionary
                      , writeDictionary
                      ) where

import Prelude hiding (readFile, writeFile)

import Data.Word (Word64)

import Data.Array.IO

import Control.Eff
import Control.Eff.Lift

import Data.Text (Text, pack, split, empty, intercalate)
import Data.Text.IO (readFile, writeFile)

newtype Dictionary = Dictionary { fromDictionary :: IOArray Word64 Text }

readDictionary :: SetMember Lift (Lift IO) r => FilePath -> Eff r Dictionary
readDictionary path = lift $ readFile path >>= parseDict
  where
    parseDict :: Text -> IO Dictionary
    parseDict t = let ls = filter (/= empty)
                         $ split (`elem` ['\r', '\n']) t
                      len = fromIntegral $ length ls
                  in fmap Dictionary $ newListArray (1, len) ls

writeDictionary :: SetMember Lift (Lift IO) r
                => FilePath
                -> Dictionary
                -> Eff r ()
writeDictionary path d = lift $ getElems (fromDictionary d)
                            >>= writeFile path
                              . intercalate (pack "\r\n")
