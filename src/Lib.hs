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

import Cluster.Kmeans
import Cluster.Words (clusterTexts, UncountedText(..), WordCount(..))
import Fetcher
import Fetcher.Rss
import Fetcher.Article

run :: IO ()
run = do
  rssLinks <- getRssLinks
  links <- fmap (concat . map (take 3)) $ runOnUrls fetchRssLinks rssLinks 
  articles <- runOnUrls fetchArticle (take 10 $ links)

  let uts = map (\(l, a) -> UncountedText l a) (zip links articles)
  let clusters = clusterTexts 6 uts

  mapM_ (print . map wcTitle) clusters

  putStrLn "Done"

