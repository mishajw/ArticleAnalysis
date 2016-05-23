module Lib (run) where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import WordCounting (clusterTexts)
import WordCountTypes (UncountedText(..), WordCount(..))
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

