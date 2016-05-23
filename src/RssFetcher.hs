{-# LANGUAGE OverloadedStrings #-}

module RssFetcher (fetchRssLinks) where

import System.IO (putStrLn)
import Data.Char (isSpace)
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (unpack)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, content, followingSibling, attribute, child, element, fromDocument, (>=>), ($//))

-- | Get the elements from a cursor
findElements :: Cursor -> [Cursor]
findElements = element "item" >=> child

-- | The data we're going to search for
findLinks :: [Cursor] -> [String]
findLinks c =
  filter (not . null) .
  map ((filter $ not.isSpace) . unpack) $
    foldMap content c

-- | Get the cursor a URL
getCursor :: String -> IO Cursor
getCursor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

-- | Fetch the links found in a RSS URL
fetchRssLinks :: String -> IO [String]
fetchRssLinks url = do
  cursor <- getCursor url
  let elems = cursor $// findElements
  let links = findLinks elems
  return links

