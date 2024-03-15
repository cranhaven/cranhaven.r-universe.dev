#ifndef GINTERVALS_META_H_INCLUDED
#define GINTERVALS_META_H_INCLUDED

#include "rdbinterval.h"

using namespace rdb;

class GIntervalsMeta {
public:
	enum Meta { STATS_FIELD, ZEROLINE_FIELD, NUM_META_FIELDS };

	virtual ~GIntervalsMeta() {}

	static const char *META_FIELD_NAMES[NUM_META_FIELDS];

	static SEXP load_meta(const char *path);
	static void save_meta(const char *path, SEXP stats, SEXP zeroline);
};

#endif
