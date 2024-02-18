#ifndef RAVETOOLS_UTILS_H
#define RAVETOOLS_UTILS_H

#include "common.h"

template <typename T>
T* get_sexp_pointer(const SEXP& x);

SEXP make_error(const char* message);

void* SEXPPOINTER(const SEXP& x);

#endif // RAVETOOLS_UTILS_H
