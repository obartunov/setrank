-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION setrank" to load this file. \quit

CREATE FUNCTION get_idf(table_name text, lexeme text)
	RETURNS float8
	AS 'MODULE_PATHNAME', 'get_idf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION get_idf(text, text) IS 'IDF of given lexeme in given stat table';

CREATE FUNCTION ts_rank_tfidf(table_name text, weights float4[], vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_tfidf_wttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_tfidf(text, float4[], tsvector, tsquery, int) IS
	'Ranking with TF/IDF';

CREATE FUNCTION ts_rank_tfidf(table_name text, weights float4[], vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_tfidf_wtt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_tfidf(text, float4[], tsvector, tsquery) IS
	'Ranking with TF/IDF';

CREATE FUNCTION ts_rank_tfidf(table_name text, vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_tfidf_ttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_tfidf(text, tsvector, tsquery, int) IS
	'Ranking with TF/IDF';

CREATE FUNCTION ts_rank_tfidf(table_name text, vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_tfidf_tt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_tfidf(text, tsvector, tsquery) IS
	'Ranking with TF/IDF';

CREATE FUNCTION ts_rank_cd_tfidf(table_name text, weights float4[], vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_cd_tfidf_wttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_cd_tfidf(text, float4[], tsvector, tsquery, int) IS
	'Cover density ranking with TF/IDF';

CREATE FUNCTION ts_rank_cd_tfidf(table_name text, weights float4[], vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_cd_tfidf_wtt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_cd_tfidf(text, float4[], tsvector, tsquery) IS
	'Cover density ranking with TF/IDF';

CREATE FUNCTION ts_rank_cd_tfidf(table_name text, vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_cd_tfidf_ttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_cd_tfidf(text, tsvector, tsquery, int) IS
	'Cover density ranking with TF/IDF';

CREATE FUNCTION ts_rank_cd_tfidf(table_name text, vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_rank_cd_tfidf_tt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_rank_cd_tfidf(text, tsvector, tsquery) IS
	'Cover density ranking with TF/IDF';

CREATE FUNCTION ts_score_tfidf(table_name text, weights float4[], vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_score_tfidf_wttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_score_tfidf(text, float4[], tsvector, tsquery, int) IS
	'Score with TF/IDF';

CREATE FUNCTION ts_score_tfidf(table_name text, weights float4[], vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_score_tfidf_wtt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_score_tfidf(text, float4[], tsvector, tsquery) IS
	'Score with TF/IDF';

CREATE FUNCTION ts_score_tfidf(table_name text, vector tsvector, query tsquery, flags int)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_score_tfidf_ttf'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_score_tfidf(text, tsvector, tsquery, int) IS
	'Score with TF/IDF';

CREATE FUNCTION ts_score_tfidf(table_name text, vector tsvector, query tsquery)
	RETURNS float4
	AS 'MODULE_PATHNAME', 'ts_score_tfidf_tt'
	LANGUAGE C IMMUTABLE;

COMMENT ON FUNCTION ts_score_tfidf(text, tsvector, tsquery) IS
	'Score with TF/IDF';


