{-# LANGUAGE OverloadedStrings #-}

module Fetcher.Article where
import Data.Text (unpack)
import Data.List (isInfixOf)
import Text.XML.Cursor (Cursor, descendant, attributeIs, content, child, element, checkName, ($//), (>=>))

import Fetcher (getCursor)

fetchArticle :: String -> IO String
fetchArticle url = do
  c <- getCursor url
  let elems = c $// getElements url
  let contents = foldMap content elems
  return $ concat . (map unpack) $ contents

getElements :: String -> Cursor -> [Cursor]
getElements url
  | "bbc.co.uk"           `isInfixOf` url = attributeIs "class" "story-body" >=> descendant
  | "theguardian.com"     `isInfixOf` url = attributeIs "itemprop" "articleBody" >=> descendant
  | "independent.co.uk"   `isInfixOf` url = attributeIs "class" "main-content-column" >=> descendant
  | otherwise = descendant

