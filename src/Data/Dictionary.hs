{-# LANGUAGE FlexibleContexts #-}
module Data.Dictionary( Dictionary (..)
                      , readDictionary
                      , writeDictionary
                      ) where

import Prelude hiding (readFile, writeFile)

import Control.Eff
import Control.Eff.Lift

import Data.Text (Text, pack, split, empty, intercalate)
import Data.Text.IO (readFile, writeFile)

import Data.Vector (toList, fromList, thaw, freeze)
import Data.Vector.Mutable (IOVector)

newtype Dictionary = Dictionary { fromDictionary :: IOVector Text }

readDictionary :: SetMember Lift (Lift IO) r => FilePath -> Eff r Dictionary
readDictionary path = lift $ readFile path >>= parseDict
  where
    parseDict :: Text -> IO Dictionary
    parseDict = fmap Dictionary
              . thaw
              . fromList
              . filter (/= empty)
              . split (`elem` ['\r', '\n'])

writeDictionary :: SetMember Lift (Lift IO) r
                => FilePath
                -> Dictionary
                -> Eff r ()
writeDictionary path d = lift $ freeze (fromDictionary d)
                            >>= writeFile path
                              . intercalate (pack "\r\n")
                              . toList
