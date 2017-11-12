--
-- The database for fluffy engine
--

--
-- create the table for true or false
--

CREATE TABLE table_true_or_false
( key_id SERIAL PRIMARY KEY
, key_body TEXT NOT NULL
, key_answer    BOOL NOT NULL
, key_rationale TEXT NULL
, key_points    INT  NOT NULL DEFAULT 1
, key_difficulty TEXT NULL
, key_references Int NULL
, key_learning_objectives TEXT NULL
, key_national_standards TEXT NULL
, key_topics TEXT NULL
, key_words TEXT[] NOT NULL DEFAULT '{}'
);

--
-- create table for gap filling
--

CREATE TABLE table_gap_filling
( key_id SERIAL PRIMARY KEY 
, key_body TEXT NOT NULL
, key_answer TEXT NOT NULL
, key_points    INT  NOT NULL DEFAULT 1
, key_difficulty TEXT NULL
, key_references Int NULL
, key_learning_objectives TEXT NULL
, key_national_standards TEXT NULL
, key_topics TEXT NULL
, key_words TEXT[] NOT NULL DEFAULT '{}'
);

--
-- create table for multiple choice
--

CREATE TABLE table_multiple_choice
( key_id SERIAL PRIMARY KEY
, key_body TEXT NOT NULL
, key_answer INT NOT NULL
, key_choices TEXT[] NOT NULL
);

