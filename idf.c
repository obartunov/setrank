#include "setrank.h"

#include "fmgr.h"

#include "access/hash.h"
#include "catalog/pg_collation.h"
#include "catalog/pg_type.h"
#include "executor/spi.h"
#include "utils/builtins.h"
#include "utils/memutils.h"
#include "utils/datum.h"


typedef struct IdfCache {
	IdfSet  *sets;
	int32   nsets;
} IdfCache;

static IdfCache SetStorage = {NULL, 0};

static uint32
hashIdfElem(const void *key, Size keysize)
{
	const IdfElem *elem = (IdfElem*)key;

	Assert(keysize == sizeof(*elem));

	return DatumGetUInt32(hash_any((const unsigned char *) elem->term, elem->termlen));
}

static int
compareIdfElem(const void *key1, const void *key2, Size keysize)
{
	const IdfElem *elem1 = (IdfElem*)key1,
		  		  *elem2 = (IdfElem*)key2;

	Assert(keysize == sizeof(*elem1));

	return varstr_cmp(elem1->term, elem1->termlen,
					  elem2->term, elem2->termlen,
					  C_COLLATION_OID);
}

static HTAB*
initHash()
{
	HASHCTL		hash_ctl;
	int			flags = 0;

	MemSet(&hash_ctl, 0, sizeof(hash_ctl));

	hash_ctl.hcxt = TopMemoryContext;
	flags |= HASH_CONTEXT;

	hash_ctl.keysize = sizeof(IdfElem);
	hash_ctl.entrysize = sizeof(IdfElem);
	flags |= HASH_ELEM;

	hash_ctl.hash = hashIdfElem;
	flags |= HASH_FUNCTION;

	hash_ctl.match = compareIdfElem;
	flags |= HASH_COMPARE;

	return hash_create("IDF storage", 1024*1024, &hash_ctl, flags);
}

static void
initIdfSet(IdfSet *set, text *tableName)
{
	MemoryContext	ctx;
	char			buf[1024];
	int				stat;
	int				i;
	Oid				ndocType;
	HASH_SEQ_STATUS	status;
	IdfElem			*elem;

	set->totaldocs = 0.0;

	snprintf(buf, sizeof(buf) - 1, "SELECT * FROM \"%s\" ORDER BY 1;", text_to_cstring(tableName));

	SPI_connect();
	stat = SPI_execute(buf, true, 0);

	if (stat != SPI_OK_SELECT)
		elog(ERROR, "SPI_execute() returns %d", stat);

	if (SPI_processed == 0)
		elog(ERROR, "Stat table '%s' is empty", text_to_cstring(tableName));


	if (SPI_tuptable->tupdesc->natts != 2)
		elog(ERROR,"Stat table is not (text, int4) nor (text, int8)");

	if (SPI_gettypeid(SPI_tuptable->tupdesc, 1) != TEXTOID)
		elog(ERROR, "Lexeme type is not a text");

	ndocType = SPI_gettypeid(SPI_tuptable->tupdesc, 2);
	if ( !(ndocType == INT4OID || ndocType == INT8OID) )
		elog(ERROR,"Stat table is not (text, int4) nor (text, int8)");

	set->hash = initHash();

	for(i=0; i<SPI_processed; i++)
	{
		bool	isnullvalue,
				isnullndoc;
		Datum	datum;
		int64	ndoc;


		if (ndocType == INT4OID)
			ndoc = DatumGetInt32(SPI_getbinval(SPI_tuptable->vals[i], SPI_tuptable->tupdesc, 2, &isnullndoc));
		else
			ndoc = DatumGetInt64(SPI_getbinval(SPI_tuptable->vals[i], SPI_tuptable->tupdesc, 2, &isnullndoc));

		if (isnullndoc)
			elog(ERROR,"NULL value in second column of table '%s'", text_to_cstring(tableName));

		if (ndoc <= 0)
			elog(ERROR,"Zero or less value in second column of table '%s'", text_to_cstring(tableName));

		datum = SPI_getbinval(SPI_tuptable->vals[i], SPI_tuptable->tupdesc, 1, &isnullvalue);

		if (isnullvalue)
		{
			if (set->totaldocs > 0)
				elog(ERROR,"Total number of document is repeated");
			set->totaldocs = ndoc;
		}
		else
		{
			bool		found = false;
			IdfElem		insert;

			insert.termlen = VARSIZE_ANY_EXHDR(datum);
			insert.term = MemoryContextAlloc(TopMemoryContext, insert.termlen);
			memcpy(insert.term, VARDATA_ANY(datum), insert.termlen);
			insert.idf = ndoc;

			hash_search(set->hash, &insert, HASH_ENTER, &found);

			if (found)
				elog(ERROR,"Values of first column of table '%s' are not unique", text_to_cstring(tableName));
		}
	}

	SPI_finish();

	if (set->totaldocs <= 0)
		elog(ERROR,"Total number of document is unknown");

	hash_seq_init(&status, set->hash);
	while ((elem = (IdfElem *) hash_seq_search(&status)) != NULL)
	{
		if (set->totaldocs < elem->idf)
			elog(ERROR,"Inconsistent data in '%s': there is values with frequency > 1", text_to_cstring(tableName));
		elem->idf = log(set->totaldocs / elem->idf);
	}

	set->defaultIdf = log(set->totaldocs / 1.0);

	ctx = MemoryContextSwitchTo(TopMemoryContext);
	set->tableName = DatumGetTextP(datumCopy(PointerGetDatum(tableName), false, -1));
	MemoryContextSwitchTo(ctx);
}

IdfSet*
getIdfSet(text *tableName)
{
	int 			i;
	IdfSet			*set;

	for(i = 0; i<SetStorage.nsets; i++)
	{
		set = SetStorage.sets + i;

		if (DatumGetBool(DirectFunctionCall2(texteq,
											PointerGetDatum(tableName),
											PointerGetDatum(set->tableName))))
			return set;
	}

	if (SetStorage.sets == NULL)
		SetStorage.sets = MemoryContextAlloc(TopMemoryContext,
											 sizeof(*SetStorage.sets));
	else
		SetStorage.sets = repalloc(SetStorage.sets,
								   sizeof(*SetStorage.sets) * (SetStorage.nsets + 1));

	set = SetStorage.sets + SetStorage.nsets;

	initIdfSet(set, tableName);

	SetStorage.nsets++;

	return set;
}

double
getIdf(IdfSet *set, char *term, int32 termlen)
{
	IdfElem	search, *elem;
	bool	found = false;

	search.term = term;
	search.termlen = termlen;

	elem = hash_search(set->hash, &search, HASH_FIND, &found);

	return (found) ? elem->idf : set->defaultIdf;
}

PG_FUNCTION_INFO_V1(get_idf);
Datum   get_idf(PG_FUNCTION_ARGS);
Datum
get_idf(PG_FUNCTION_ARGS)
{
	text 	*tableName = PG_GETARG_TEXT_P(0);
	text 	*term = PG_GETARG_TEXT_P(1);
	IdfSet	*set = getIdfSet(tableName);


	PG_RETURN_FLOAT8(getIdf(set, VARDATA_ANY(term), VARSIZE_ANY_EXHDR(term)));
}

