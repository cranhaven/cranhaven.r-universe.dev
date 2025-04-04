#ifndef _RTWOBITLIB_UTILS_H_
#define _RTWOBITLIB_UTILS_H_

#include <Rdefines.h>

#include <kent/twoBit.h>

const char *_filepath2str(SEXP filepath);

struct twoBitFile *_open_2bit_file(SEXP filepath);

#endif  /* _RTWOBITLIB_UTILS_H_ */

