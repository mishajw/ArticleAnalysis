{-# LANGUAGE OverloadedStrings #-}
module DB where

import Data.String.Utils (strip)
import Data.List.Split (splitOn)
import qualified Data.Text as DT (pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

defaultConnection = open "res/database.db"

-- | Setup the database tables
setup :: IO ()
setup = runFile "res/sql/setup.sql"

-- | Run the SQL in a file
runFile :: FilePath -> IO ()
runFile path = do
  statements <- readFile path
  let statementList = map DT.pack . filter (not.null) . map strip $ splitOn ";" statements
  conn <- defaultConnection
  mapM ((execute_ conn) . Query) $ statementList
  return ()

