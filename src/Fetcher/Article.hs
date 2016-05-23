{-# LANGUAGE OverloadedStrings #-}

module Fetcher.Article where
import Data.Text (unpack)
import Text.XML.Cursor (descendant, content, child, element, checkName, ($//), (>=>))

import Fetcher (getCursor)

fetchArticle :: String -> IO String
fetchArticle url = do
  c <- getCursor url
  let elems = c $// descendant
  let contents = foldMap content elems
  return $ concat . (map unpack) $ contents

