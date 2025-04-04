#include "Rtwobitlib_utils.h"

#include <kent/twoBit.h>

const char *_filepath2str(SEXP filepath)
{
	SEXP path;

	if (!IS_CHARACTER(filepath) || LENGTH(filepath) != 1)
		error("'filepath' must be a single string");
	path = STRING_ELT(filepath, 0);
	if (path == NA_STRING)
		error("'filepath' cannot be NA");
	return CHAR(path);
}

struct twoBitFile *_open_2bit_file(SEXP filepath)
{
	return twoBitOpen(_filepath2str(filepath));
}

