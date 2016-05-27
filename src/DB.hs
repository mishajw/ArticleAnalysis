{-# LANGUAGE OverloadedStrings #-}
module DB where

import Data.String.Utils (strip)
import Data.List.Split (splitOn)
import qualified Data.Text as DT (pack)
import Data.Int (Int)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Cluster.Words

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
  mapM_ (execute_ conn . Query) statementList
  return ()

insertWordCount :: WordCount -> IO ()
insertWordCount (WordCount t cs) = do
  conn <- defaultConnection
  execute conn "INSERT INTO page (name) VALUES (?)" $ Only t
  pageId <- fmap fromIntegral $ lastInsertRowId conn

  mapM_ (\(w, c) -> do
    wordId <- insertWord conn w
    execute conn "INSERT INTO page_word (page_id, word_id, count) VALUES (?, ?, ?)" (PageWordRow pageId wordId c)) cs

insertWord :: Connection -> String -> IO Int
insertWord conn w = do
  results <- query conn "SELECT id FROM word WHERE word = ?" (Only w)

  case results of
    [] -> do
      execute conn "INSERT INTO word (word) VALUES (?)" (Only w)
      fmap fromIntegral $ lastInsertRowId conn
    (Only id) : _ -> return id

getWordCount :: String -> IO WordCount
getWordCount title = do
  conn <- defaultConnection
  results <- query conn " \
    \  SELECT w.word, pw.count \
    \  FROM \
    \    word w, \
    \    page_word pw, \
    \    ( SELECT id FROM page \
    \      WHERE name = ? \
    \      LIMIT 1 \
    \    ) AS p \
    \  WHERE \
    \    pw.page_id = p.id AND \
    \    pw.word_id = w.id" (Only title)

  return $ WordCount title $ map (\(WordCountRow w c) -> (w, c)) results


data PageWordRow = PageWordRow Int Int Int deriving Show
instance FromRow PageWordRow where
  fromRow = PageWordRow <$> field <*> field <*> field
instance ToRow PageWordRow where
  toRow (PageWordRow pid wid count) = toRow (pid, wid, count)

data WordCountRow = WordCountRow String Int deriving Show
instance FromRow WordCountRow where
  fromRow = WordCountRow <$> field <*> field
instance ToRow WordCountRow where
  toRow (WordCountRow pid wid) = toRow (pid, wid)

-- instance FromRow Int where
--   fromRow = Int <$> field
-- instance ToRow Int where
--   toRow i = i

