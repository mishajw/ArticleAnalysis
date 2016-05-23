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
  links <- runOnUrls fetchRssLinks rssLinks 
  mapM_ (print . length) links

  articles <- runOnUrls fetchArticle (take 3 . concat $ links)
  mapM_ putStrLn articles

  putStrLn "Done"

