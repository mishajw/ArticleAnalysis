{-# LANGUAGE OverloadedStrings #-}
module DB (
  defaultConnection
, setup
, insertWordCount
, insertWordCounts 
, insertContents
, getWordCount
, getAllWordCounts
, getAllContents
) where

import Data.String.Utils (strip)
import Data.List.Split (splitOn)
import qualified Data.Text as DT (pack)
import Database.SQLite.Simple

import Cluster.Words

defaultConnection = open "res/database.db"

-- | Setup the database tables
setup :: Connection -> IO ()
setup conn = runFile conn "res/sql/setup.sql"

-- | Run the SQL in a file
runFile :: Connection -> FilePath -> IO ()
runFile conn path = do
  statements <- readFile path
  let statementList = map DT.pack . filter (not.null) . map strip $ splitOn ";" statements
  mapM_ (execute_ conn . Query) statementList
  return ()

-- | Insert a WordCount
insertWordCount :: Connection -> WordCount -> IO ()
insertWordCount conn (WordCount t cs) = do
  execute conn "INSERT INTO page (name) VALUES (?)" $ Only t
  pageId <- fromIntegral <$> lastInsertRowId conn

  mapM_ (\(w, c) -> do
    wordId <- insertWord conn w
    execute conn "INSERT INTO page_word (page_id, word_id, count) VALUES (?, ?, ?)" (PageWordRow pageId wordId c)) cs

-- | Insert multiple word counts
insertWordCounts :: Connection -> [WordCount] -> IO ()
insertWordCounts conn wcs =
  withTransaction conn $ mapM_ (insertWordCount conn) wcs

-- | Insert a word and return it's ID, or return the ID of an existing word
insertWord :: Connection -> String -> IO Int
insertWord conn w = do
  results <- query conn "SELECT id FROM word WHERE word = ?" (Only w)

  case results of
    [] -> do
      execute conn "INSERT INTO word (word) VALUES (?)" (Only w)
      fromIntegral <$> lastInsertRowId conn
    Only id : _ -> return id

insertContents :: Connection -> UncountedText -> IO ()
insertContents conn (UncountedText t c) = do
  pageId <- insertPage conn t
  execute conn "INSERT INTO page_content (page_id, content) VALUES (?, ?)" (PageContentRow pageId c)

insertPage :: Connection -> String -> IO Int
insertPage conn t = do
  results <- query conn "SELECT id FROM page WHERE name = ?" (Only t)

  case results of
    [] -> do
      execute conn "INSERT INTO page (name) VALUES (?)" $ Only t
      fromIntegral <$> lastInsertRowId conn
    Only id : _ -> return id

-- | Get a word count of a page
getWordCount :: Connection -> String -> IO WordCount
getWordCount conn title = do
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
getAllWordCounts :: Connection -> IO [WordCount]
getAllWordCounts conn = do
  results <- query_ conn "SELECT name FROM page"
  mapM (\(Only n) -> getWordCount conn n) results

getAllContents :: Connection -> IO [UncountedText]
getAllContents conn =
  query_ conn "\ 
    \ SELECT P.name, C.content \
    \ FROM page P, page_content C \
    \ WHERE P.id = C.page_id"

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

data PageContentRow = PageContentRow Int String deriving Show
instance FromRow PageContentRow where
  fromRow = PageContentRow <$> field <*> field
instance ToRow PageContentRow where
  toRow (PageContentRow pid contents) = toRow (pid, contents)

instance FromRow UncountedText where
  fromRow = UncountedText <$> field <*> field
instance ToRow UncountedText where
  toRow (UncountedText pid contents) = toRow (pid, contents)

