{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Main( main
           ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, void)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.RegexShow
import Data.Traversable (traverse)
import System.Environment (getArgs)

-- array
import Data.Array.IO

-- extensible-effects
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

-- regex-posix
import Text.Regex.Posix.String

-- system-random-effect
import System.Random.Effect (Random, mkRandomIO, uniformIntDist)

-- text
import Data.Text (Text, pack, unpack, empty)
import Data.Text.Read (decimal)

import Data.Dictionary
import Data.Usage

uniformIntegral :: (Integral a, Member (State Random) r) => a -> a -> Eff r a
uniformIntegral l h = fromInteger <$> uniformIntDist (toInteger l) (toInteger h)

-- | This is slow. Write a mutable array version.
knuthShuffle :: (Ix i, Integral i, Member (State Random) r, SetMember Lift (Lift IO) r)
             => IOArray i a
             -> Eff r ()
knuthShuffle a = do
      (l, h) <- lift $ getBounds a
      swaps <- replicateM (rangeSize (l, h)) (uniformIntegral l h)
      traverse_ swap $ zip [l..h] swaps
  where
    swap (i, j) = lift $ do
        [vi, vj] <- traverse (readArray a) [i, j]
        traverse_ (uncurry $ writeArray a) [(i, vj), (j, vi)]

withDecimal :: Integral a => Text -> (a -> b) -> b
withDecimal t f = case decimal t of
        Right (len, remain) -> if remain == empty
                               then f len
                               else error $ "length has tail " <> show t
        _ -> error $ "error parsing length " <> show t

compileSimple :: String -> IO Regex
compileSimple = fmap (either (error . show) id) . compile compBlank execBlank

createUsage :: (SetMember Lift (Lift IO) r, Member (State Random) r)
            => IO (Usage (Eff r ()))
createUsage = do
      [shuffleRegex, genRegex] <- traverse (compileSimple `withShow`) ["shuffle", "gen"]
      [shuffleUse, genUse] <- sequence [shuffle, gen]
      return $ Uses
        [ ((shuffleRegex, shuffleDesc), shuffleUse)
        , ((genRegex, genDesc), genUse)
        ]

genDesc, shuffleDesc :: Text
genDesc = pack "Generate a password"
shuffleDesc = pack "Shuffle a dictionary file"

getDictionary :: IO (RegexShow String Regex, Text)
getDictionary = do
      regex <- compileSimple `withShow` ".*"
      return (regex, pack "Dictionary file to use")

gen :: (Member (State Random) r, SetMember Lift (Lift IO) r)
    => IO (Usage (Text -> Eff r ()))
gen = do
      askDict <- getDictionary
      num <- compileSimple `withShow` "[0-9]\\+"
      return $ Uses [( askDict
                     , Uses [((num, pack "Password length"), Done genLen)]
                    )]
  where
    genLen tlen tdict _ = withDecimal tlen
                        $ \len -> do
                            dict <- readDictionary $ unpack tdict
                            pass <- replicateM len (randomWord dict)
                            lift $ putStrLn $ intercalate " " pass

    randomWord :: (SetMember Lift (Lift IO) r, Member (State Random) r)
               => Dictionary
               -> Eff r String
    randomWord (Dictionary ws) = do
              i <- randomIndex ws
              lift $ unpack <$> readArray ws i

    randomIndex a = lift (getBounds a) >>= uncurry uniformIntegral

shuffle :: (Member (State Random) r, SetMember Lift (Lift IO) r)
        => IO (Usage (Text -> Eff r ()))
shuffle = do
      askDict <- getDictionary
      return $ Uses [(askDict, Done shuffleDict)]
  where
    shuffleDict tdict _ = do
        let sdict = unpack tdict
        dict <- readDictionary sdict
        knuthShuffle $ fromDictionary dict
        writeDictionary sdict dict

main :: IO ()
main = do
    args <- getArgs
    rndgen <- runLift mkRandomIO
    usage <- createUsage
    (getUse (pack <$> args) usage)
      >>= maybe (showUsage usage)
                (void . runLift . runState rndgen)

showUsage :: Usage a -> IO ()
showUsage usage = do
    putStrLn "Usage:"
    putStrLn $ indent $ show usage
  where
    indent = intercalate "\n" . fmap ("  " <>) . lines
