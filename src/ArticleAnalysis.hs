module ArticleAnalysis (run) where

import Cluster.Kmeans
import Cluster.Words (clusterTexts, UncountedText(..), WordCount(..), mkWordCount)
import Fetcher
import Fetcher.Rss
import Fetcher.Article
import DB

run :: IO ()
run = do
  as <- getNewArticles
  mapM_ print as
  -- insertArticles
  -- clusters <- clusterFromDB
  --
  -- mapM_ (print . length) clusters
  return ()

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
  rssLinks <- take 1 <$> getRssLinks
  articleLinks <- take 10 . concat <$> runOnUrls fetchRssLinks rssLinks
  articles <- runOnUrls fetchArticle articleLinks
  return $ zipWith UncountedText articleLinks articles

