module ArticleAnalysis (run) where

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
  uts <- getNewArticles
  let wcs = map mkWordCount uts

  conn <- defaultConnection
  setup conn

  mapM_ (insertContents conn) uts
  insertWordCounts conn wcs

getNewArticles :: IO [UncountedText]
getNewArticles = do
  rssLinks <- getRssLinks
  articleLinks <- concat <$> runOnUrls fetchRssLinks rssLinks
  articles <- runOnUrls fetchArticle articleLinks
  return $ zipWith UncountedText articleLinks articles

