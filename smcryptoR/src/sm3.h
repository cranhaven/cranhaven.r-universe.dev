#ifndef SM3_H
#define SM3_H

#include <Rinternals.h>

SEXP sm3_hash_wrapper(SEXP msg);
SEXP sm3_hash_string_wrapper(SEXP msg_str);
SEXP sm3_hash_file_wrapper(SEXP file_path);

#endif /* SM3_H */
