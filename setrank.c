#include <postgres.h>
#include <limits.h>
#include <math.h>

#include "setrank.h"
#include "tsearch/ts_utils.h"
#include "utils/array.h"
#include "miscadmin.h"

PG_MODULE_MAGIC;

static const double weights[] = {0.1f, 0.2f, 0.4f, 1.0f};

#define wpos(wep)	( w[ WEP_GETWEIGHT(wep) ] )

#define RANK_NO_NORM			0x00
#define RANK_NORM_LOGLENGTH		0x01
#define RANK_NORM_LENGTH		0x02
#define RANK_NORM_EXTDIST		0x04
#define RANK_NORM_UNIQ			0x08
#define RANK_NORM_LOGUNIQ		0x10
#define RANK_NORM_RDIVRPLUS1	0x20
#define DEF_NORM_METHOD			RANK_NO_NORM

static const double *
getWeights(ArrayType *win)
{
	static double ws[lengthof(weights)];
	int			i;
	float4	   *arrdata;

	if (win == NULL)
		return weights;

	if (ARR_NDIM(win) != 1)
		ereport(ERROR,
				(errcode(ERRCODE_ARRAY_SUBSCRIPT_ERROR),
				 errmsg("array of weight must be one-dimensional")));

	if (ArrayGetNItems(ARR_NDIM(win), ARR_DIMS(win)) < lengthof(weights))
		ereport(ERROR,
				(errcode(ERRCODE_ARRAY_SUBSCRIPT_ERROR),
				 errmsg("array of weight is too short")));

	if (array_contains_nulls(win))
		ereport(ERROR,
				(errcode(ERRCODE_NULL_VALUE_NOT_ALLOWED),
				 errmsg("array of weight must not contain nulls")));

	arrdata = (float4 *) ARR_DATA_PTR(win);
	for (i = 0; i < lengthof(weights); i++)
	{
		ws[i] = (arrdata[i] >= 0) ? arrdata[i] : weights[i];
		if (ws[i] > 1.0)
			ereport(ERROR,
					(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
					 errmsg("weight out of range")));
	}

	return ws;
}

static int
cnt_length(TSVector t)
{
	WordEntry  *ptr = ARRPTR(t),
				*end = (WordEntry *) STRPTR(t);
	int         len = 0;

	while (ptr < end)
	{
		int         clen = POSDATALEN(t, ptr);

		if (clen == 0)
			len += 1;
		else
			len += clen;

		ptr++;
	}

	return len;
}

static double
word_distance(int32 w)
{
	if (w > 100)
		return 1e-30f;

	return 1.0 / (1.005 + 0.05 * exp(((double) w) / 1.5 - 2));
}

#define WordECompareQueryItem(e,q,p,i,m) \
	tsCompareString((q) + (i)->distance, (i)->length,   \
					(e) + (p)->pos, (p)->len, (m))


static int
compareQueryOperand(const void *a, const void *b, void *arg)
{
	char       *operand = (char *) arg;
	QueryOperand *qa = (*(QueryOperand *const *) a);
	QueryOperand *qb = (*(QueryOperand *const *) b);

	return tsCompareString(operand + qa->distance, qa->length,
							operand + qb->distance, qb->length,
							false);
}


static WordEntry *
find_wordentry(TSVector t, TSQuery q, QueryOperand *item, int32 *nitem)
{
	WordEntry  *StopLow = ARRPTR(t);
	WordEntry  *StopHigh = (WordEntry *) STRPTR(t);
	WordEntry  *StopMiddle = StopHigh;
	int         difference;

	*nitem = 0;

	/* Loop invariant: StopLow <= item < StopHigh */
	while (StopLow < StopHigh)
	{
		StopMiddle = StopLow + (StopHigh - StopLow) / 2;
		difference = WordECompareQueryItem(STRPTR(t), GETOPERAND(q), StopMiddle, item, false);
		if (difference == 0)
		{
			StopHigh = StopMiddle;
			*nitem = 1;
			break;
		}
		else if (difference > 0)
			StopLow = StopMiddle + 1;
		else
			StopHigh = StopMiddle;
	}

	if (item->prefix)
	{
		if (StopLow >= StopHigh)
			StopMiddle = StopHigh;

		*nitem = 0;

		while (StopMiddle < (WordEntry *) STRPTR(t) &&
			WordECompareQueryItem(STRPTR(t), GETOPERAND(q), StopMiddle, item, true) == 0)
		{
			(*nitem)++;
			StopMiddle++;
		}
	}

	return (*nitem > 0) ? StopHigh : NULL;
}

static QueryOperand **
SortAndUniqItems(TSQuery q, int *size)
{
	char       *operand = GETOPERAND(q);
	QueryItem  *item = GETQUERY(q);
	QueryOperand **res,
				**ptr,
				**prevptr;

	ptr = res = (QueryOperand **) palloc(sizeof(QueryOperand *) * *size);

	/* Collect all operands from the tree to res */
	while ((*size)--)
	{
		if (item->type == QI_VAL)
		{
			*ptr = (QueryOperand *) item;
			ptr++;
		}
		item++;
	}

	*size = ptr - res;
	if (*size < 2)
		return res;

	qsort_arg(res, *size, sizeof(QueryOperand *), compareQueryOperand, (void *) operand);

	ptr = res + 1;
	prevptr = res;

	/* remove duplicates */
	while (ptr - res < *size)
	{
		if (compareQueryOperand((void *) ptr, (void *) prevptr, (void *) operand) != 0)
		{
			prevptr++;
			*prevptr = *ptr;
		}
		ptr++;
	}

	*size = prevptr + 1 - res;
	return res;
}

static double
maxTF(TSVector t)
{
	int 	i;
	int	maxnpos = 1;

	for(i=0; i<t->size; i++)
	{
		WordEntry 	*e = ARRPTR(t) + i;
		int			npos = POSDATALEN(t, e);

		if (npos > maxnpos)
			maxnpos = npos;
	}

	return 1 + log(maxnpos);
}

static double calc_rank_or(IdfSet *set, const double *w, TSVector t, TSQuery q);
static double calc_rank_and(IdfSet *set, const double *w, TSVector t, TSQuery q);

static double
calc_rank_and(IdfSet *set, const double *w, TSVector t, TSQuery q)
{
	WordEntryPosVector **pos;
	WordEntryPosVector1 posnull;
	WordEntryPosVector *POSNULL;
	int			i,
				k,
				l,
				p;
	WordEntry  *entry,
			   *firstentry;
	WordEntryPos *post,
			   *ct;
	int32		dimt,
				lenct,
				dist,
				nitem;
	double		res = -1.0;
	QueryOperand **item;
	int			size = q->size;
	double		maxtf;

	item = SortAndUniqItems(q, &size);
	if (size < 2)
	{
		pfree(item);
		return calc_rank_or(set, w, t, q);
	}

	pos = (WordEntryPosVector **) palloc0(sizeof(WordEntryPosVector *) * q->size);
	maxtf = maxTF(t);

	/* A dummy WordEntryPos array to use when haspos is false */
	posnull.npos = 1;
	posnull.pos[0] = 0;
	WEP_SETPOS(posnull.pos[0], MAXENTRYPOS - 1);
	POSNULL = (WordEntryPosVector *) &posnull;

	for (i = 0; i < size; i++)
	{
		firstentry = entry = find_wordentry(t, q, item[i], &nitem);
		if (!entry)
			continue;

		while (entry - firstentry < nitem)
		{
			double	entryWeight;

			if (entry->haspos)
				pos[i] = _POSVECPTR(t, entry);
			else
				pos[i] = POSNULL;

			dimt = pos[i]->npos;
			post = pos[i]->pos;

			entryWeight = (1 + log(dimt)) * getIdf(set, STRPTR(t) + entry->pos, entry->len) /
						(maxtf * set->defaultIdf /* use as max IDF */ );

			for (k = 0; k < i; k++)
			{
				if (!pos[k])
					continue;
				lenct = pos[k]->npos;
				ct = pos[k]->pos;

				for (l = 0; l < dimt; l++)
				{
					for (p = 0; p < lenct; p++)
					{
						dist = Abs((int) WEP_GETPOS(post[l]) - (int) WEP_GETPOS(ct[p]));
						if (dist || (dist == 0 && (pos[i] == POSNULL || pos[k] == POSNULL)))
						{
							double		curw;

							if (!dist)
								dist = MAXENTRYPOS;

							curw = sqrt(wpos(post[l]) * wpos(ct[p]) * word_distance(dist) * entryWeight);
							res = (res < 0) ? curw : 1.0 - (1.0 - res) * (1.0 - curw);
						}
					}
				}
			}

			entry++;
		}
	}
	pfree(pos);
	pfree(item);
	return res;
}

static double
calc_rank_or(IdfSet *set, const double *w, TSVector t, TSQuery q)
{
	WordEntry  *entry,
			   *firstentry;
	WordEntryPosVector1 posnull;
	WordEntryPos *post;
	int32		dimt,
				j,
				i,
				nitem;
	double		res = 0.0;
	QueryOperand **item;
	int			size = q->size;
	double		maxtf = maxTF(t);

	/* A dummy WordEntryPos array to use when haspos is false */
	posnull.npos = 1;
	posnull.pos[0] = 0;

	item = SortAndUniqItems(q, &size);

	for (i = 0; i < size; i++)
	{
		double		resj,
					wjm;
		int32		jm;

		firstentry = entry = find_wordentry(t, q, item[i], &nitem);
		if (!entry)
			continue;

		while (entry - firstentry < nitem)
		{
			if (entry->haspos)
			{
				dimt = POSDATALEN(t, entry);
				post = POSDATAPTR(t, entry);
			}
			else
			{
				dimt = posnull.npos;
				post = posnull.pos;
			}

			resj = 0.0;
			wjm = -1.0;
			jm = 0;
			for (j = 0; j < dimt; j++)
			{
				resj = resj + wpos(post[j]) / ((j + 1) * (j + 1));
				if (wpos(post[j]) > wjm)
				{
					wjm = wpos(post[j]);
					jm = j;
				}
			}

			resj *= (1 + log(dimt)) * getIdf(set, STRPTR(t) + entry->pos, entry->len) /
					(maxtf * set->defaultIdf /* use as max IDF */ );
/*
			limit (sum(i/i^2),i->inf) = pi^2/6
			resj = sum(wi/i^2),i=1,noccurence,
			wi - should be sorted desc,
			don't sort for now, just choose maximum weight. This should be corrected
			Oleg Bartunov
*/
			res = res + (wjm + resj - wjm / ((jm + 1) * (jm + 1))) / 1.64493406685;

			entry++;
		}
	}
	if (size > 0)
		res = res / size;
	pfree(item);
	return res;
}

static double
calc_rank(text *statTable, const double *w, TSVector t, TSQuery q, int32 method)
{
	QueryItem  *item = GETQUERY(q);
	double		res = 0.0;
	int			len;
	IdfSet      *set = getIdfSet(statTable);

	if (!t->size || !q->size)
		return 0.0;

	/* XXX: What about NOT? */
	res = (item->type == QI_OPR && item->qoperator.oper == OP_AND) ?
		calc_rank_and(set, w, t, q) : calc_rank_or(set, w, t, q);

	if (res < 0)
		res = 1e-20f;

	if ((method & RANK_NORM_LOGLENGTH) && t->size > 0)
		res /= log((double) (cnt_length(t) + 1)) / log(2.0);

	if (method & RANK_NORM_LENGTH)
	{
		len = cnt_length(t);
		if (len > 0)
			res /= (double) len;
	}

	/* RANK_NORM_EXTDIST not applicable */

	if ((method & RANK_NORM_UNIQ) && t->size > 0)
		res /= (double) (t->size);

	if ((method & RANK_NORM_LOGUNIQ) && t->size > 0)
		res /= log((double) (t->size + 1)) / log(2.0);

	if (method & RANK_NORM_RDIVRPLUS1)
		res /= (res + 1);

	return res;
}

PG_FUNCTION_INFO_V1(ts_rank_tfidf_wttf);
Datum   ts_rank_tfidf_wttf(PG_FUNCTION_ARGS);
Datum
ts_rank_tfidf_wttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	int			method = PG_GETARG_INT32(4);
	float		res;

	res = calc_rank(statTable, getWeights(win), txt, query, method);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_tfidf_wtt);
Datum   ts_rank_tfidf_wtt(PG_FUNCTION_ARGS);
Datum
ts_rank_tfidf_wtt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	float		res;

	res = calc_rank(statTable, getWeights(win), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_tfidf_ttf);
Datum   ts_rank_tfidf_ttf(PG_FUNCTION_ARGS);
Datum
ts_rank_tfidf_ttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	int			method = PG_GETARG_INT32(3);
	float		res;

	res = calc_rank(statTable, getWeights(NULL), txt, query, method);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_tfidf_tt);
Datum   ts_rank_tfidf_tt(PG_FUNCTION_ARGS);
Datum
ts_rank_tfidf_tt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	float		res;

	res = calc_rank(statTable, getWeights(NULL), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

typedef struct
{
	QueryItem **item;
	int16		nitem;
	uint8		wclass;
	int32		pos;
	WordEntry	*entry;
} DocRepresentation;

static int
compareDocR(const void *va, const void *vb)
{
	const DocRepresentation *a = (const DocRepresentation *) va;
	const DocRepresentation *b = (const DocRepresentation *) vb;

	if (a->pos == b->pos)
		return 0;
	return (a->pos > b->pos) ? 1 : -1;
}

typedef struct
{
	TSQuery		query;
	bool	   *operandexist;
} QueryRepresentation;

#define QR_GET_OPERAND_EXISTS(q, v)		( (q)->operandexist[ ((QueryItem*)(v)) - GETQUERY((q)->query) ] )
#define QR_SET_OPERAND_EXISTS(q, v)  QR_GET_OPERAND_EXISTS(q,v) = true

static bool
checkcondition_QueryOperand(void *checkval, QueryOperand *val)
{
	QueryRepresentation *qr = (QueryRepresentation *) checkval;

	return QR_GET_OPERAND_EXISTS(qr, val);
}

typedef struct
{
	int			pos;
	int			p;
	int			q;
	DocRepresentation *begin;
	DocRepresentation *end;
} CoverExt;


static bool
Cover(DocRepresentation *doc, int len, QueryRepresentation *qr, CoverExt *ext)
{
	DocRepresentation *ptr;
	int			lastpos = ext->pos;
	int			i;
	bool		found = false;

	/*
	 * since this function recurses, it could be driven to stack overflow.
	 * (though any decent compiler will optimize away the tail-recursion.
	 */
	check_stack_depth();

	memset(qr->operandexist, 0, sizeof(bool) * qr->query->size);

	ext->p = INT_MAX;
	ext->q = 0;
	ptr = doc + ext->pos;

	/* find upper bound of cover from current position, move up */
	while (ptr - doc < len)
	{
		for (i = 0; i < ptr->nitem; i++)
		{
			if (ptr->item[i]->type == QI_VAL)
				QR_SET_OPERAND_EXISTS(qr, ptr->item[i]);
		}
		if (TS_execute(GETQUERY(qr->query), (void *) qr, false, checkcondition_QueryOperand))
		{
			if (ptr->pos > ext->q)
			{
				ext->q = ptr->pos;
				ext->end = ptr;
				lastpos = ptr - doc;
				found = true;
			}
			break;
		}
		ptr++;
	}

	if (!found)
		return false;

	memset(qr->operandexist, 0, sizeof(bool) * qr->query->size);

	ptr = doc + lastpos;

	/* find lower bound of cover from found upper bound, move down */
	while (ptr >= doc + ext->pos)
	{
		for (i = 0; i < ptr->nitem; i++)
			if (ptr->item[i]->type == QI_VAL)
				QR_SET_OPERAND_EXISTS(qr, ptr->item[i]);
		if (TS_execute(GETQUERY(qr->query), (void *) qr, true, checkcondition_QueryOperand))
		{
			if (ptr->pos < ext->p)
			{
				ext->begin = ptr;
				ext->p = ptr->pos;
			}
			break;
		}
		ptr--;
	}

	if (ext->p <= ext->q)
	{
		/*
		 * set position for next try to next lexeme after beginning of found
		 * cover
		 */
		ext->pos = (ptr - doc) + 1;
		return true;
	}

	ext->pos++;
	return Cover(doc, len, qr, ext);
}

static DocRepresentation *
get_docrep(TSVector txt, QueryRepresentation *qr, int *doclen)
{
	QueryItem  *item = GETQUERY(qr->query);
	WordEntry  *entry,
			   *firstentry;
	WordEntryPos *post;
	int32		dimt,
				j,
				i,
				nitem;
	int			len = qr->query->size * 4,
				cur = 0;
	DocRepresentation *doc;
	char	   *operand;

	doc = (DocRepresentation *) palloc(sizeof(DocRepresentation) * len);
	operand = GETOPERAND(qr->query);

	for (i = 0; i < qr->query->size; i++)
	{
		QueryOperand *curoperand;

		if (item[i].type != QI_VAL)
			continue;

		curoperand = &item[i].qoperand;

		if (QR_GET_OPERAND_EXISTS(qr, &item[i]))
			continue;

		firstentry = entry = find_wordentry(txt, qr->query, curoperand, &nitem);
		if (!entry)
			continue;

		while (entry - firstentry < nitem)
		{
			if (entry->haspos)
			{
				dimt = POSDATALEN(txt, entry);
				post = POSDATAPTR(txt, entry);
			}
			else
			{
				/* ignore words without positions */
				entry++;
				continue;
			}

			while (cur + dimt >= len)
			{
				len *= 2;
				doc = (DocRepresentation *) repalloc(doc, sizeof(DocRepresentation) * len);
			}

			for (j = 0; j < dimt; j++)
			{
				if (j == 0)
				{
					int			k;

					doc[cur].nitem = 0;
					doc[cur].item = (QueryItem **) palloc(sizeof(QueryItem *) * qr->query->size);

					for (k = 0; k < qr->query->size; k++)
					{
						QueryOperand *kptr = &item[k].qoperand;
						QueryOperand *iptr = &item[i].qoperand;

						if (k == i ||
							(item[k].type == QI_VAL &&
							 compareQueryOperand(&kptr, &iptr, operand) == 0))
						{
							/*
							 * if k == i, we've already checked above that
							 * it's type == Q_VAL
							 */
							doc[cur].item[doc[cur].nitem] = item + k;
							doc[cur].nitem++;
							QR_SET_OPERAND_EXISTS(qr, item + k);
						}
					}
				}
				else
				{
					doc[cur].nitem = doc[cur - 1].nitem;
					doc[cur].item = doc[cur - 1].item;
				}
				doc[cur].pos = WEP_GETPOS(post[j]);
				doc[cur].wclass = WEP_GETWEIGHT(post[j]);
				doc[cur].entry = entry;
				cur++;
			}

			entry++;
		}
	}

	*doclen = cur;

	if (cur > 0)
	{
		qsort((void *) doc, cur, sizeof(DocRepresentation), compareDocR);
		return doc;
	}

	pfree(doc);
	return NULL;
}

static double
calc_rank_cd(text *statTable, const double *arrdata, TSVector txt, TSQuery query, int method)
{
	DocRepresentation *doc;
	int			i,
				doclen = 0;
	CoverExt	ext;
	double		Wdoc = 0.0;
	double		wordWeights[lengthof(weights)];
	double		SumDist = 0.0,
				PrevExtPos = 0.0,
				CurExtPos = 0.0;
	int			NExtent = 0;
	QueryRepresentation qr;
	IdfSet		*set = getIdfSet(statTable);
	double		maxtf = maxTF(txt);

	for (i = 0; i < lengthof(weights); i++)
	{
		wordWeights[i] = ((double) ((arrdata[i] >= 0) ? arrdata[i] : weights[i]));
		if (wordWeights[i] > 1.0)
			ereport(ERROR,
					(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
					 errmsg("weight out of range")));
	}

	qr.query = query;
	qr.operandexist = (bool *) palloc0(sizeof(bool) * query->size);

	doc = get_docrep(txt, &qr, &doclen);
	if (!doc)
	{
		pfree(qr.operandexist);
		return 0.0;
	}

	MemSet(&ext, 0, sizeof(CoverExt));
	while (Cover(doc, doclen, &qr, &ext))
	{
		double		Cpos = 0.0;
		double		InvSum = 0.0;
		int			nNoise;
		DocRepresentation *ptr = ext.begin;

		while (ptr <= ext.end)
		{
			double idf = getIdf(set, STRPTR(txt) + ptr->entry->pos, ptr->entry->len);
			double tf = log( ((double)POSDATALEN(txt, ptr->entry)) );
			double tfidf = (1.0 + tf) * idf / (maxtf * set->defaultIdf /* as max */ );

			InvSum += 1.0 / (wordWeights[ptr->wclass] * tfidf) ;

			ptr++;
		}

		Cpos = ((double) (ext.end - ext.begin + 1)) / InvSum;

		/*
		 * if doc are big enough then ext.q may be equal to ext.p due to limit
		 * of posional information. In this case we approximate number of
		 * noise word as half cover's length
		 */
		nNoise = (ext.q - ext.p) - (ext.end - ext.begin);
		if (nNoise < 0)
			nNoise = (ext.end - ext.begin) / 2;
		Wdoc += Cpos * word_distance (1 + nNoise);

		CurExtPos = ((double) (ext.q + ext.p)) / 2.0;
		if (NExtent > 0 && CurExtPos > PrevExtPos		/* prevent devision by
														 * zero in a case of
														 * multiple lexize */ )
			SumDist += 1.0 / (CurExtPos - PrevExtPos);

		PrevExtPos = CurExtPos;
		NExtent++;
	}

	if ((method & RANK_NORM_LOGLENGTH) && txt->size > 0)
		Wdoc /= log((double) (cnt_length(txt) + 1));

	if (method & RANK_NORM_LENGTH)
	{
		int len =  cnt_length(txt);

		if (len > 0)
			Wdoc /= (double) len;
	}

	if ((method & RANK_NORM_EXTDIST) && NExtent > 0 && SumDist > 0)
		Wdoc /= ((double) NExtent) / SumDist;

	if ((method & RANK_NORM_UNIQ) && txt->size > 0)
		Wdoc /= (double) (txt->size);

	if ((method & RANK_NORM_LOGUNIQ) && txt->size > 0)
		Wdoc /= log((double) (txt->size + 1)) / log(2.0);

	if (method & RANK_NORM_RDIVRPLUS1)
		Wdoc /= (Wdoc + 1);

	pfree(doc);

	pfree(qr.operandexist);

	return Wdoc;
}

PG_FUNCTION_INFO_V1(ts_rank_cd_tfidf_wttf);
Datum   ts_rank_cd_tfidf_wttf(PG_FUNCTION_ARGS);
Datum
ts_rank_cd_tfidf_wttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	int			method = PG_GETARG_INT32(4);
	float		res;

	res = calc_rank_cd(statTable, getWeights(win), txt, query, method);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_cd_tfidf_wtt);
Datum   ts_rank_cd_tfidf_wtt(PG_FUNCTION_ARGS);
Datum
ts_rank_cd_tfidf_wtt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	float		res;

	res = calc_rank_cd(statTable, getWeights(win), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_cd_tfidf_ttf);
Datum   ts_rank_cd_tfidf_ttf(PG_FUNCTION_ARGS);
Datum
ts_rank_cd_tfidf_ttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	int			method = PG_GETARG_INT32(3);
	float		res;

	res = calc_rank_cd(statTable, getWeights(NULL), txt, query, method);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_rank_cd_tfidf_tt);
Datum   ts_rank_cd_tfidf_tt(PG_FUNCTION_ARGS);
Datum
ts_rank_cd_tfidf_tt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	float		res;

	res = calc_rank_cd(statTable, getWeights(NULL), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

static double
calc_score(text *statTable, const double *arrdata, TSVector txt, TSQuery query, int method)
{
	DocRepresentation *doc;
	int			len,
				i,
				doclen = 0;
	CoverExt	ext;
	double		Wdoc = 0.0;
	double		wordWeights[lengthof(weights)];
	double		SumDist = 0.0,
				PrevExtPos = 0.0,
				CurExtPos = 0.0;
	int			NExtent = 0;
	QueryRepresentation qr;
	IdfSet		*set = getIdfSet(statTable);
	double		maxtf = maxTF(txt);
	/* Added by SK */
	int *cover_keys = (int *)palloc(0);
	int *cover_lengths = (int *)palloc(0);
	double *cover_ranks = (double *)palloc(0);
	int ncovers = 0;

	for (i = 0; i < lengthof(weights); i++)
	{
		wordWeights[i] = ((double) ((arrdata[i] >= 0) ? arrdata[i] : weights[i]));
		if (wordWeights[i] > 1.0)
			ereport(ERROR,
					(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
					 errmsg("weight out of range")));
	}

	qr.query = query;
	qr.operandexist = (bool *) palloc0(sizeof(bool) * query->size);

	doc = get_docrep(txt, &qr, &doclen);
	if (!doc)
	{
		pfree(qr.operandexist);
		return 0.0;
	}

	MemSet(&ext, 0, sizeof(ext));
	while (Cover(doc, doclen, &qr, &ext))
	{
		double		Cpos = 0.0;
		double		InvSum = 0.0;
		int			nNoise;
		DocRepresentation *ptr = ext.begin;
		/* Added by SK */
		int new_cover_idx = 0;
		int new_cover_key = 0;
		QueryItem  *item = GETQUERY(qr.query);
		int nitems = 0;

		while (ptr <= ext.end)
		{
			double idf = getIdf(set, STRPTR(txt) + ptr->entry->pos, ptr->entry->len);
			double tf = log( ((double)POSDATALEN(txt, ptr->entry)) );
			double tfidf = (1.0 + tf) * idf / (maxtf * set->defaultIdf /* as max */ );

			InvSum += 1.0 / (wordWeights[ptr->wclass] * tfidf) ;

			/* SK: Quick and dirty hash key. Hope collisions will be not too frequent. */
			new_cover_key = new_cover_key << 1;
			new_cover_key += (int)ptr->item;
			ptr++;
		}

		/* Added by SK */
		/* TODO: use binary tree?.. */
		while(new_cover_idx < ncovers) {
			if(new_cover_key == cover_keys[new_cover_idx])
				break;
			new_cover_idx ++;
		}

		if(new_cover_idx == ncovers) {
			cover_keys = (int *)repalloc(cover_keys, sizeof(int)*(ncovers + 1));
			cover_lengths = (int *)repalloc(cover_lengths, sizeof(int)*(ncovers + 1));
			cover_ranks = (double *)repalloc(cover_ranks, sizeof(double)*(ncovers + 1));

			cover_lengths[ncovers] = 0;
			cover_ranks[ncovers] = 0;

			ncovers ++;
		}

		cover_keys[new_cover_idx] = new_cover_key;

		/* Compute the number of query terms in the cover */
		for (i = 0; i < qr.query->size; i++)
			if (item[i].type == QI_VAL &&
				QR_GET_OPERAND_EXISTS(&qr, &item[i]))
				nitems ++;

		Cpos = ((double) (ext.end - ext.begin + 1)) / InvSum;

		if (nitems > 0)
			Cpos *= nitems;

		/*
		 * if doc are big enough then ext.q may be equal to ext.p due to limit
		 * of posional information. In this case we approximate number of
		 * noise word as half cover's length
		 */
		nNoise = (ext.q - ext.p) - (ext.end - ext.begin);
		if (nNoise < 0)
			nNoise = (ext.end - ext.begin) / 2;
		/* SK: Wdoc += Cpos / ((double) (1 + nNoise)); */
		cover_lengths[new_cover_idx] ++;
		cover_ranks[new_cover_idx] += Cpos * word_distance(1 + nNoise)
			/ cover_lengths[new_cover_idx] / cover_lengths[new_cover_idx]  / 1.64493406685;

		CurExtPos = ((double) (ext.q + ext.p)) / 2.0;
		if (NExtent > 0 && CurExtPos > PrevExtPos		/* prevent devision by
														 * zero in a case of
				multiple lexize */ )
			SumDist += 1.0 / (CurExtPos - PrevExtPos);

		PrevExtPos = CurExtPos;
		NExtent++;
	}

	/* Added by SK */
	for(i = 0; i < ncovers; i++)
		Wdoc += cover_ranks[i];

	if ((method & RANK_NORM_LOGLENGTH) && txt->size > 0)
		Wdoc /= log((double) (cnt_length(txt) + 1));

	if (method & RANK_NORM_LENGTH)
	{
		len = cnt_length(txt);
		if (len > 0)
			Wdoc /= (double) len;
	}

	if ((method & RANK_NORM_EXTDIST) && NExtent > 0 && SumDist > 0)
		Wdoc /= ((double) NExtent) / SumDist;

	if ((method & RANK_NORM_UNIQ) && txt->size > 0)
		Wdoc /= (double) (txt->size);

	if ((method & RANK_NORM_LOGUNIQ) && txt->size > 0)
		Wdoc /= log((double) (txt->size + 1)) / log(2.0);

	if (method & RANK_NORM_RDIVRPLUS1)
		Wdoc /= (Wdoc + 1);

	pfree(doc);

	pfree(qr.operandexist);

	pfree(cover_keys);
	pfree(cover_lengths);
	pfree(cover_ranks);

	return Wdoc;
}

PG_FUNCTION_INFO_V1(ts_score_tfidf_wttf);
Datum   ts_score_tfidf_wttf(PG_FUNCTION_ARGS);
Datum
ts_score_tfidf_wttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	int			method = PG_GETARG_INT32(4);
	float		res;

	res = calc_score(statTable, getWeights(win), txt, query, method);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_score_tfidf_wtt);
Datum   ts_score_tfidf_wtt(PG_FUNCTION_ARGS);
Datum
ts_score_tfidf_wtt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	ArrayType  *win = (ArrayType *) PG_DETOAST_DATUM(PG_GETARG_DATUM(1));
	TSVector	txt = PG_GETARG_TSVECTOR(2);
	TSQuery		query = PG_GETARG_TSQUERY(3);
	float		res;

	res = calc_score(statTable, getWeights(win), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(win, 1);
	PG_FREE_IF_COPY(txt, 2);
	PG_FREE_IF_COPY(query, 3);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_score_tfidf_ttf);
Datum   ts_score_tfidf_ttf(PG_FUNCTION_ARGS);
Datum
ts_score_tfidf_ttf(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	int			method = PG_GETARG_INT32(3);
	float		res;

	res = calc_score(statTable, getWeights(NULL), txt, query, method);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

PG_FUNCTION_INFO_V1(ts_score_tfidf_tt);
Datum   ts_score_tfidf_tt(PG_FUNCTION_ARGS);
Datum
ts_score_tfidf_tt(PG_FUNCTION_ARGS)
{
	text    	*statTable = PG_GETARG_TEXT_P(0);
	TSVector	txt = PG_GETARG_TSVECTOR(1);
	TSQuery		query = PG_GETARG_TSQUERY(2);
	float		res;

	res = calc_score(statTable, getWeights(NULL), txt, query, DEF_NORM_METHOD);

	PG_FREE_IF_COPY(txt, 1);
	PG_FREE_IF_COPY(query, 2);
	PG_RETURN_FLOAT4(res);
}

