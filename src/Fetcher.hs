module Fetcher (openUrl) where

import qualified Data.ByteString.Char8 as B
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import Text.XML.HXT.Core
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Network.HTTP
import Network.URI
import System.Environment

openUrl :: String -> MaybeT IO String
openUrl url = case parseURI url of
  Nothing -> fail ""
  Just u  -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

get :: String -> IO (IOSArrow XmlTree (NTree XNode))
get url = do
  contents <- runMaybeT $ openUrl url
  return $ readString [withParseHTML yes, withWarnings no] (fromMaybe "" contents)

