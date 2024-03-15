/*
 * GenomeCheckVtrack.cpp
 *
 *  Created on: Mar 8, 2012
 *      Author: hoichman
 */

#include "rdbutils.h"
#include "TrackExpressionVars.h"

using namespace std;
using namespace rdb;

extern "C" {

SEXP gcheck_vtrack(SEXP _vtrack, SEXP _envir)
{
	try {
		RdbInitializer rdb_init;

		// check the arguments
		if (!isString(_vtrack) || length(_vtrack) != 1)
			verror("Virtual track argument is not a string");

		const char *vtrack = CHAR(STRING_ELT(_vtrack, 0));
		vector<string> exprs;
		IntervUtils iu(_envir);
		TrackExpressionVars parser(iu);

		exprs.push_back(vtrack);
		parser.parse_exprs(exprs);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

}
