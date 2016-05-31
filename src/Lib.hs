module Lib (run) where

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
run = do
  insertArticles
  clusters <- clusterFromDB
  
  mapM_ (print . length) clusters

clusterFromDB :: IO [[WordCount]]
clusterFromDB = do
  conn <- defaultConnection
  wcs <- getAllWordCounts conn

  return $ kmeans 6 wcs

insertArticles :: IO ()
insertArticles = do
  wcs <- getNewArticles

  conn <- defaultConnection
  setup conn
  insertWordCounts conn wcs

getNewArticles :: IO [WordCount]
getNewArticles = do
  rssLinks <- getRssLinks
  articleLinks <- concat <$> runOnUrls fetchRssLinks rssLinks
  articles <- runOnUrls fetchArticle articleLinks

  let uts = zipWith UncountedText articleLinks articles
  return $ map mkWordCount uts

