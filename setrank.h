#ifndef SETRANK_H
#define SETRANK_H

#include <postgres.h>
#include <utils/hsearch.h>

typedef struct IdfElem {
	char	*term;
	int32	termlen;
	double	idf; /*  log(d/df) */
} IdfElem;

typedef struct IdfSet {
	text			*tableName;
	double			totaldocs;
	double			defaultIdf;
	HTAB			*hash;
} IdfSet;

extern IdfSet* getIdfSet(text *tableName);
extern double getIdf(IdfSet *set, char *term, int32 termlen);

#endif
