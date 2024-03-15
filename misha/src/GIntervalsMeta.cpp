#include "GIntervalsMeta.h"
#include "GIntervalsMeta1D.h"
#include "GIntervalsMeta2D.h"
#include "rdbutils.h"

const char *GIntervalsMeta::META_FIELD_NAMES[NUM_META_FIELDS] = { "stats", "zeroline" };

SEXP GIntervalsMeta::load_meta(const char *path)
{
	string filename(path);
	filename += "/.meta";
	SEXP meta = RSaneUnserialize(filename.c_str());
	rprotect(meta);
	if (!isVector(meta) || length(meta) != 2)
		verror("Invalid format of meta file %s", filename.c_str());

	SEXP chromsizes = VECTOR_ELT(meta, 0);
	if (!isVector(chromsizes) || (length(chromsizes) != GIntervalsMeta1D::NUM_STAT_COLS && length(chromsizes) != GIntervalsMeta2D::NUM_STAT_COLS))
		verror("Invalid format of meta file %s", filename.c_str());

	return meta;
}

void GIntervalsMeta::save_meta(const char *path, SEXP stats, SEXP zeroline)
{
	SEXP meta;
	SEXP colnames;

	rprotect(meta = RSaneAllocVector(VECSXP, NUM_META_FIELDS));
    rprotect(colnames = RSaneAllocVector(STRSXP, NUM_META_FIELDS));

	SET_VECTOR_ELT(meta, STATS_FIELD, stats);
	SET_VECTOR_ELT(meta, ZEROLINE_FIELD, zeroline);

	for (int i = 0; i < NUM_META_FIELDS; ++i) 
		SET_STRING_ELT(colnames, i, mkChar(META_FIELD_NAMES[i]));

    setAttrib(meta, R_NamesSymbol, colnames);

	string filename(path);
	filename += "/.meta";
	RSaneSerialize(meta, filename.c_str());
}
