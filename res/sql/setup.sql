DROP TABLE IF EXISTS page;
DROP TABLE IF EXISTS word;
DROP TABLE IF EXISTS page_word;
DROP TABLE IF EXISTS page_content;

CREATE TABLE page (
  id INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE word (
  id INTEGER PRIMARY KEY,
  word TEXT UNIQUE
);

CREATE TABLE page_word (
  id INTEGER PRIMARY KEY,
  page_id INTEGER,
  word_id INTEGER,
  count INTEGER,

  FOREIGN KEY (page_id) REFERENCES page(id),
  FOREIGN KEY (word_id) REFERENCES word(id)
);

CREATE TABLE page_content (
  id INTEGER PRIMARY KEY,
  page_id INTEGER,
  content TEXT,

  FOREIGN KEY (page_id) REFERENCES page(id)
);

