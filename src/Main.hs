{-# LANGUAGE FlexibleContexts #-}
module Main( main
           ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, void)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.RegexShow
import Data.Traversable (traverse)
import System.Environment (getArgs)

-- extensible-effects
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

-- regex-posix
import Text.Regex.Posix.String

-- system-random-effect
import System.Random.Effect

-- text
import Data.Text (Text, pack, unpack, empty)
import Data.Text.Read (decimal)

-- vector
import qualified Data.Vector.Mutable as V (length, read)

import Data.Dictionary
import Data.Usage

withDecimal :: Integral a => Text -> (a -> b) -> b
withDecimal t f = case decimal t of
        Right (len, remain) -> if remain == empty
                               then f len
                               else error $ "length has tail " <> show t
        _ -> error $ "error parsing length " <> show t

compileSimple :: String -> IO Regex
compileSimple = fmap (either (error . show) id) . compile compBlank execBlank

main :: IO ()
main = do
    args <- getArgs
    rndgen <- runLift mkRandomIO
    usage <- createUsage
    (getUse (pack <$> args) usage)
      >>= maybe (showUsage usage)
                (void . runLift . runState rndgen)

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
      askLength <- getLength
      return $ Uses [(askDict, Uses [(askLength, Done genLen)])
                    ]
  where
    getLength = do
      num <- compileSimple `withShow` "[0-9]\\+"
      return (num, pack "Password length")

    genLen tlen tdict _ = withDecimal tlen
                        $ \len -> do
                            dict <- readDictionary $ unpack tdict
                            pass <- replicateM len (randomEntry dict)
                            lift $ putStrLn $ intercalate " " pass

    randomEntry :: (SetMember Lift (Lift IO) r, Member (State Random) r)
                => Dictionary
                -> Eff r String
    randomEntry (Dictionary ws) = do
              i <- uniformIntegralDist 0 $ V.length ws - 1
              lift $ unpack <$> V.read ws i

shuffle :: (Member (State Random) r, SetMember Lift (Lift IO) r)
        => IO (Usage (Text -> Eff r ()))
shuffle = do
      askDict <- getDictionary
      return $ Uses [(askDict, Done shuffleDict)]
  where
    shuffleDict tdict _ = do
        let sdict = unpack tdict
        dict <- readDictionary sdict
        knuthShuffleM $ fromDictionary dict
        writeDictionary sdict dict

showUsage :: Usage a -> IO ()
showUsage usage = do
    putStrLn "Usage:"
    putStrLn $ indent $ show usage
  where
    indent = intercalate "\n" . fmap ("  " <>) . lines
