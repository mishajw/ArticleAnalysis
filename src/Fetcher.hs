module Fetcher (getCursor) where

import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

-- | Get the cursor a URL
getCursor :: String -> IO Cursor
getCursor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

