{-# LANGUAGE OverloadedStrings #-}

module Fetcher.Article where
import Data.Text (unpack)
import Data.List (isInfixOf)
import Text.XML.Cursor (Cursor, descendant, attributeIs, content, child, checkName, ($//), (>=>))

import Fetcher (getCursor)

fetchArticle :: String -> IO String
fetchArticle url = do
  c <- getCursor url
  let elems = c $// getElements url >=> descendant >=> checkName (not . (`elem` ["script", "style"])) >=> child
  let contents = foldMap content elems
  return $ concatMap unpack contents

getElements :: String -> Cursor -> [Cursor]
getElements url
  | "bbc.co.uk"         `isInfixOf` url = attributeIs "class"    "story-body"
  | "theguardian.com"   `isInfixOf` url = attributeIs "itemprop" "articleBody"
  | "independent.co.uk" `isInfixOf` url = attributeIs "class"    "main-content-column"
  | otherwise = pure

