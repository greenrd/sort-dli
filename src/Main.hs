{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative      ((<$>))
import           Control.Monad            (guard)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Conduit             (Conduit, Sink, Source, mergeSource,
                                           (=$=))
import           Data.Conduit.Combinators (filterM)
import           Data.Conduit.Internal    (zipConduitApp)
import           Data.Conduit.List        (mapM, mapM_, mapMaybe, sourceList)
import qualified Data.Conduit.List        as DCL
import           Data.List                (find)
import           Data.Text                (Text, concat, pack, toLower, unpack)
import           Data.XML.Types           (Content (..), Event (..), Name,
                                           nameLocalName)
import           Network.HTTP.Conduit     (parseUrl)
import           Network.HTTP.Simple      (httpSink)
import           Network.URI              (parseURIReference, pathSegments)
import           Prelude                  hiding (concat)
import           System.Directory         (doesFileExist)
import           System.Environment       (getArgs)
import           System.FilePath          (takeExtension)
import           Text.HTML.DOM            (eventConduit)

outputDir :: FilePath
outputDir = "sorted"

htmlNameEq :: Text -> Name -> Bool
htmlNameEq t = (== lt) . toLower . nameLocalName
  where lt = toLower t

contentToText :: Content -> Text
contentToText (ContentText t) = t
contentToText (ContentEntity t) = t

extractImages :: Monad m => Conduit Event m Text
extractImages = mapMaybe extract
  where extract :: Event -> Maybe Text
        extract evt = do
          EventBeginElement name atts <- return evt
          guard $ htmlNameEq "img" name
          imgSrcAttr <- find (htmlNameEq "src" . fst) atts
          let imgSrc = concat . map contentToText . snd $ imgSrcAttr
          uri <- parseURIReference $ unpack imgSrc
          return . pack . last $ pathSegments uri

destFilenames :: [Text]
destFilenames = do
  a <- ['a'..'z']
  b <- ['a'..'z']
  return $ pack [a, b]

destFilenamesSource :: Monad m => Source m Text
destFilenamesSource = sourceList destFilenames

-- spy :: (MonadIO m, Show a) => Text -> Conduit a m a
-- spy message = DCL.iterM $ \x -> liftIO . putStrLn $ unpack message ++ ": " ++ show x

processConduit :: FilePath -> Sink Event IO ()
processConduit dir =
  extractImages =$= DCL.map fullImgFilename =$= filterM exists =$= mergeSource destFilenamesSource =$= DCL.map (uncurry genCmd) =$= DCL.mapM_ (putStrLn . unpack)
  where exists :: Text -> IO Bool
        exists = doesFileExist . unpack
        genCmd :: Text -> Text -> Text
        genCmd destFilename full = concat [ "mv '",
                                            full,
                                           "' '",
                                            pack outputDir,
                                            "/",
                                            destFilename,
                                            fileExt full,
                                            "'"
                                          ]
        fullImgFilename :: Text -> Text
        fullImgFilename imgFilename = concat [pack dir, "/", imgFilename]
        fileExt :: Text -> Text
        fileExt = pack . takeExtension . unpack

sortDli :: String -> FilePath -> IO ()
sortDli urlStr dir = do
  putStrLn $ "mkdir " ++ outputDir
  url <- parseUrl urlStr
  httpSink url $ \_ -> eventConduit =$= processConduit dir

main :: IO ()
main = do
  args <- getArgs
  case args of
    [url] -> sortDli url "."
    [url, dir] -> sortDli url dir
