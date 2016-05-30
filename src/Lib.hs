module Lib (
  run
, module Cluster.Kmeans
, module Cluster.Words
, module Fetcher
, module Fetcher.Rss
, module Fetcher.Article
) where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Control.Concurrent.ParallelIO

import Cluster.Kmeans
import Cluster.Words (clusterTexts, UncountedText(..), WordCount(..), mkWordCount)
import Fetcher
import Fetcher.Rss
import Fetcher.Article
import DB

run :: IO ()
run = insertArticles

clusterFromDB :: IO ()
clusterFromDB = do
  return()
  conn <- defaultConnection
  wcs <- getAllWordCounts conn

  let clusters = kmeans 6 wcs

  mapM_ (print . length . map wcTitle) clusters

  putStrLn "Done"

insertArticles :: IO ()
insertArticles = do
  rssLinks <- getRssLinks
  articleLinks <- concat <$> runOnUrls fetchRssLinks rssLinks
  articles <- runOnUrls fetchArticle articleLinks

  let uts = map (\(t, w) -> UncountedText t w) (zip articleLinks articles)
  let wcs = map mkWordCount uts
  
  conn <- defaultConnection
  insertWordCounts conn wcs

