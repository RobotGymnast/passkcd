{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module Data.Usage( Usage (..)
                 , getUse
                 , compile
                 ) where

import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import Text.Regex.Posix.String

import Data.RegexShow

type Description = Text

type SingleUse a = ((RegexShow String Regex, Description), Usage (Text -> a))
data Usage a = Done a
             | Uses [SingleUse a]
  deriving (Functor)

instance Show (Usage a) where
  show = unpack . intercalate (pack "\n") . textShow
    where
      textShow :: Usage a -> [Text]
      textShow (Done _) = []
      textShow (Uses l) = l
                      >>= \((regex, desc), u) -> pack (show regex) <> pack ": " <> desc
                                               : fmap (pack "  " <>) (textShow u)

firstJust :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJust _ [] = return Nothing
firstJust f (a:as) = f a >>= maybe (firstJust f as) (return . Just)

getUse :: [Text]
       -> Usage a
       -> IO (Maybe a)
getUse [] (Done a) = return (Just a)
getUse _ (Done _) = return Nothing
getUse [] (Uses _) = return Nothing
getUse (a:as) (Uses us) = firstJust tryMatch us
  where
    m `when` mb = mb >>= \b -> if b then m else return Nothing

    tryMatch ((regex, _), use) = fmap (fmap ($ a)) (getUse as use) `when` (regex `matches` a)

    regex `matches` t = do
                  r <- regex `withoutShow` (`execute` unpack t)
                  return $ case r of
                            Right (Just _) -> True
                            Right Nothing -> False
                            Left err -> error $ show err
