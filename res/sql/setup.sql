CREATE TABLE IF NOT EXISTS page (
  id INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE IF NOT EXISTS word (
  id INTEGER PRIMARY KEY,
  word TEXT UNIQUE
);

CREATE TABLE IF NOT EXISTS page_word (
  id INTEGER PRIMARY KEY,
  page_id INTEGER,
  word_id INTEGER,
  count INTEGER,

  FOREIGN KEY (page_id) REFERENCES page(id),
  FOREIGN KEY (word_id) REFERENCES word(id)
);

CREATE TABLE IF NOT EXISTS page_content (
  id INTEGER PRIMARY KEY,
  page_id INTEGER,
  content TEXT,

  FOREIGN KEY (page_id) REFERENCES page(id)
);

