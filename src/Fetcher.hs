{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Prelude hiding (concat, putStrLn)
import Data.Text (concat)
import Data.Text.IO (putStrLn){-/hi-}
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, content, attribute, child, element, fromDocument, (>=>), ($//), (&//), (&/), (&|))

-- The URL we're going to search
url = "http://feeds.bbci.co.uk/news/rss.xml"

-- The data we're going to search for
findNodes :: Cursor -> [Cursor]
findNodes = element "item" &/ element "title" >=> child

-- Extract the data from each node in turn
extractData = concat . content

-- Process the list of data elements
processData = mapM_ putStrLn

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

main = do
     cursor <- cursorFor url 
     processData $ cursor $// findNodes &| extractData

