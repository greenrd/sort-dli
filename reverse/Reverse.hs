{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Conduit             (Conduit, Sink, await, yield, ($$),
                                           (=$=))
import           Data.Conduit.Combinators (linesUnbounded, stdin)
import qualified Data.Conduit.Combinators as DCC
import           Data.Text                (Text, unwords, words)
import           Data.Text.IO             (putStrLn)
import           Prelude                  hiding (lines, putStrLn, unwords,
                                           words)

-- | Moves the first input to the end of the output and applies the first function;
-- applies the second function to the rest
rotate1 :: Monad m => (a -> a) -> (a -> a) -> Conduit a m a
rotate1 f g = maybe (return ()) ((\x -> DCC.map g >> yield x) . f) =<< await

-- A sequence of words
type Line = [Text]

reverseMkdir :: Line -> Line
reverseMkdir ["mkdir", x] = ["rmdir", x]

reverseMv :: Line -> Line
reverseMv ["mv", src, dest] = ["mv", dest, src]

lines :: Monad m => Conduit Text m Line
lines = linesUnbounded =$= DCC.map words

process :: Sink Text IO ()
process = lines =$= rotate1 reverseMkdir reverseMv =$= DCC.mapM_ (putStrLn . unwords)

main :: IO ()
main = stdin $$ process

