-- | Regex type maintaining the show instance from its precompiled form.
module Data.RegexShow( RegexShow
                     , withShow
                     , withoutShow
                     ) where

data RegexShow s r = RegexShow s r

instance Show s => Show (RegexShow s r) where
  show (RegexShow s _) = show s

withShow :: Functor f => (s -> f r) -> s -> f (RegexShow s r)
withShow f s = fmap (RegexShow s) (f s)

withoutShow :: RegexShow s r -> (r -> a) -> a
withoutShow (RegexShow _ r) f = f r
