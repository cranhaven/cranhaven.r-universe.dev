#include "twobit_roundtrip.h"
#include "Rtwobitlib_utils.h"

#include <kent/hash.h> /* for newHash(), freeHash(), hashLookup(), hashAdd() */
#include <kent/dnautil.h>  /* for dnaUtilOpen() */
#include <kent/dnaseq.h>  /* for dnaSeqFree() */
#include <kent/twoBit.h>

#include <stdio.h>  /* for fopen(), fclose() */
#include <string.h>  /* for strerror() */


/****************************************************************************
 * C_twobit_read()
 */

static SEXP load_sequence_as_CHARSXP(struct twoBitFile *tbf, char *name)
{
	struct dnaSeq *seq;
	int n;
	SEXP ans;

	/* twoBitReadSeqFragExt() loads the sequence data in memory. */
	seq = twoBitReadSeqFragExt(tbf, name, 0, 0, TRUE, &n);
	ans = PROTECT(mkCharLen(seq->dna, n));
	dnaSeqFree(&seq);
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP C_twobit_read(SEXP filepath)
{
	struct twoBitFile *tbf;
	int ans_len, i;
	SEXP ans, ans_names, tmp;
	struct twoBitIndex *index;

	tbf = _open_2bit_file(filepath);

	ans_len = tbf->seqCount;
	ans = PROTECT(NEW_CHARACTER(ans_len));
	ans_names = PROTECT(NEW_CHARACTER(ans_len));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);

	for (i = 0, index = tbf->indexList;
	     i < ans_len;
	     i++, index = index->next)
	{
		if (index == NULL) {  /* should never happen */
			twoBitClose(&tbf);
			UNPROTECT(1);
			error("Rtwobitlib internal error in "
			      "C_twobit_read():\n"
			      "    index == NULL");
		}
		tmp = PROTECT(mkChar(index->name));
		SET_STRING_ELT(ans_names, i, tmp);
		UNPROTECT(1);
		tmp = PROTECT(load_sequence_as_CHARSXP(tbf, index->name));
		SET_STRING_ELT(ans, i, tmp);
		UNPROTECT(1);
	}

	twoBitClose(&tbf);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * C_twobit_write()
 */

/* Returns -1 if error, 1 if sequence must be skipped, 0 otherwise. */
static int check_input_sequence(SEXP x_elt, SEXP x_names_elt,
				boolean skip_dups, struct hash *uniqHash,
				const char **msg)
{
	/* max seqname length is 255 for 2bit format */
	static char msg_buf[280];
	const char *seqname;

	*msg = msg_buf;
	if (x_elt == NA_STRING) {
		*msg = "'x' cannot contain NAs";
		return -1;  /* error */
	}
	if (x_names_elt == NA_STRING) {
		*msg = "the names on 'x' cannot contain NAs";
		return -1;  /* error */
	}
	if (LENGTH(x_names_elt) == 0) {
		*msg = "the names on 'x' cannot contain empty strings";
		return -1;  /* error */
	}
	seqname = CHAR(x_names_elt);
	if (LENGTH(x_elt) == 0) {
		snprintf(msg_buf, sizeof(msg_buf),
			 "sequence %s has length 0", seqname);
		return 1;  /* skip sequence with warning */
	}
	if (hashLookup(uniqHash, seqname)) {
		snprintf(msg_buf, sizeof(msg_buf),
			 "duplicate sequence name %s", seqname);
		if (skip_dups)
			return 1;  /* skip sequence with warning */
		return -1;  /* error */
	}
	hashAdd(uniqHash, seqname, NULL);
	return 0;
}

static struct twoBit *make_twoBitList_from_STRSXP(SEXP x, boolean skip_dups)
{
	SEXP x_names, x_elt, x_names_elt;
	struct twoBit *twoBitList = NULL, *twoBit;
	struct hash *uniqHash;
	int x_len, i, ret;
	struct dnaSeq seq;
	const char *msg;

	if (!IS_CHARACTER(x))
		error("'x' must be a character vector");
	x_names = GET_NAMES(x);
	if (!IS_CHARACTER(x_names))
		error("'x' must have names");
	uniqHash = newHash(18);
	x_len = LENGTH(x);
	for (i = 0; i < x_len; i++) {
		x_elt = STRING_ELT(x, i);
		x_names_elt = STRING_ELT(x_names, i);
		ret = check_input_sequence(x_elt, x_names_elt,
					   skip_dups, uniqHash, &msg);
		if (ret < 0) {
			freeHash(&uniqHash);
			twoBitFreeList(&twoBitList);
			error("%s", msg);
		}
		if (ret > 0) {
			warning("%s ==> skipping it", msg);
			continue;
		}

		/* We discard the 'const' qualifier to avoid a compilation
		   warning. Safe to do here because twoBitFromDnaSeq() will
		   actually treat 'seq.dna' and 'seq.name' as 'const char *'. */
		seq.dna = (char *) CHAR(x_elt);
		seq.name = (char *) CHAR(x_names_elt);
		seq.size = LENGTH(x_elt);

		twoBit = twoBitFromDnaSeq(&seq, TRUE);
		slAddHead(&twoBitList, twoBit);
	}
	freeHash(&uniqHash);
	slReverse(&twoBitList);
	return twoBitList;
}

/* --- .Call ENTRY POINT --- */
SEXP C_twobit_write(SEXP x, SEXP filepath, SEXP use_long, SEXP skip_dups)
{
	const char *path;
	struct twoBit *twoBitList, *twoBit;
	FILE *f;
	int ret;
	const char *msg;

	path = _filepath2str(filepath);

	dnaUtilOpen();

	/* Preprocess the data to write. */
	twoBitList = make_twoBitList_from_STRSXP(x, LOGICAL(skip_dups)[0]);

	/* Open destination file. */
	f = fopen(path, "wb");
	if (f == NULL) {
		twoBitFreeList(&twoBitList);
		error("cannot open %s to write: %s", path, strerror(errno));
	}

	/* Write data to destination file. */
	ret = twoBitWriteHeaderExt(twoBitList, f, LOGICAL(use_long)[0], &msg);
	if (ret < 0) {
		fclose(f);
		twoBitFreeList(&twoBitList);
		if (ret != -2 || LOGICAL(use_long)[0])
			error("%s", msg);
		/* index overflow error */
		error("%s\nCall twobit_write() again "
		      "with 'use.long=TRUE'.", msg);
	}

	for (twoBit = twoBitList; twoBit != NULL; twoBit = twoBit->next)
		twoBitWriteOne(twoBit, f);

	/* Close file and free memory. */
	fclose(f);
	twoBitFreeList(&twoBitList);
	return R_NilValue;
}

