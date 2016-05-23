module Fetcher (getCursor, runOnUrls) where

import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)
import Control.Concurrent.ParallelIO

runOnUrls :: (String -> IO a) -> [String] -> IO [a]
runOnUrls f urls = do
  parallel $ map f urls

-- | Get the cursor a URL
getCursor :: String -> IO Cursor
getCursor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

