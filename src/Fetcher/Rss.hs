{-# LANGUAGE OverloadedStrings #-}

module Fetcher.Rss (fetchRssLinks) where

import System.IO (putStrLn)
import Data.Char (isSpace)
import Data.Text (unpack)
import Text.XML.Cursor (Cursor, content, child, element, (>=>), ($//))

import Fetcher (getCursor)

-- | Get the elements from a cursor
findElements :: Cursor -> [Cursor]
findElements = element "item" >=> child

-- | The data we're going to search for
findLinks :: [Cursor] -> [String]
findLinks c =
  filter (not . null) .
  map ((filter $ not.isSpace) . unpack) $
    foldMap content c

-- | Fetch the links found in a RSS URL
fetchRssLinks :: String -> IO [String]
fetchRssLinks url = do
  cursor <- getCursor url
  let elems = cursor $// findElements
  let links = findLinks elems
  return links

