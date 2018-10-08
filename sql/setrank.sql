CREATE EXTENSION setrank;

SELECT 'l_' || i::text AS l, i AS n INTO ts  FROM generate_series(1,100) AS i;
INSERT INTO ts VALUES (NULL, 100);

SELECT get_idf('ts', 'l_1'); 
SELECT get_idf('ts', 'l_10'); 
SELECT get_idf('ts', 'l_50'); 
SELECT get_idf('ts', 'l_99'); 
SELECT get_idf('ts', 'x'); 

SELECT ts_rank_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2');
SELECT ts_rank_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'x');
SELECT ts_rank_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_99');

SELECT ts_rank_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 & l_99');
SELECT ts_rank_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 | l_99');

SELECT ts_rank_cd_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2');
SELECT ts_rank_cd_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'x');
SELECT ts_rank_cd_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_99');

SELECT ts_rank_cd_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 & l_99');
SELECT ts_rank_cd_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 | l_99');

SELECT ts_score_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2');
SELECT ts_score_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'x');
SELECT ts_score_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_99');

SELECT ts_score_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 & l_99');
SELECT ts_score_tfidf('ts', 'l_2:2,5 x:3,4 l_99:1,6', 'l_2 | l_99');

