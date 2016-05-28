{-# LANGUAGE OverloadedStrings #-}
module DB (
  setup
, insertWordCount
, insertWordCounts 
, getWordCount
, getAllWordCounts
) where

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

-- | Insert a WordCount
insertWordCount :: WordCount -> IO ()
insertWordCount (WordCount t cs) = do
  conn <- defaultConnection
  execute conn "INSERT INTO page (name) VALUES (?)" $ Only t
  pageId <- fromIntegral <$> lastInsertRowId conn

  mapM_ (\(w, c) -> do
    wordId <- insertWord conn w
    execute conn "INSERT INTO page_word (page_id, word_id, count) VALUES (?, ?, ?)" (PageWordRow pageId wordId c)) cs

insertWordCounts :: [WordCount] -> IO ()
insertWordCounts wcs = do
  conn <- defaultConnection
  withTransaction conn $ mapM_ insertWordCount wcs

-- | Insert a word and return it's ID, or return the ID of an existing word
insertWord :: Connection -> String -> IO Int
insertWord conn w = do
  results <- query conn "SELECT id FROM word WHERE word = ?" (Only w)

  case results of
    [] -> do
      execute conn "INSERT INTO word (word) VALUES (?)" (Only w)
      fromIntegral <$> lastInsertRowId conn
    Only id : _ -> return id

-- | Get a word count of a page
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

-- | Get all word counts in the database
getAllWordCounts :: IO [WordCount]
getAllWordCounts = do
  conn <- defaultConnection
  results <- query_ conn "SELECT name FROM page"
  mapM (\(Only n) -> getWordCount n) results

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

