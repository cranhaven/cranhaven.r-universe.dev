#include <Rinternals.h>
#include "smcrypto/api.h"
#include "sm3.h"

SEXP sm3_hash_wrapper(SEXP msg) {
  if (TYPEOF(msg) != RAWSXP) {
    Rf_error("msg must be a raw vector");
  }
  const unsigned char* msg_raw = RAW(msg);
  char* hash = sm3_hash(msg_raw, XLENGTH(msg));
  SEXP result = Rf_ScalarString(Rf_mkCharCE(hash, CE_UTF8));
  free_char_array(hash);
  return result;
}

SEXP sm3_hash_string_wrapper(SEXP msg_str) {
  if (TYPEOF(msg_str) != STRSXP) {
    Rf_error("msg_str must be a character string");
  }
  const char* msg_str_c = CHAR(STRING_ELT(msg_str, 0));
  char* hash = sm3_hash_string(msg_str_c);
  SEXP result = Rf_ScalarString(Rf_mkCharCE(hash, CE_UTF8));
  free_char_array(hash);
  return result;
}

SEXP sm3_hash_file_wrapper(SEXP file_path) {
  if (TYPEOF(file_path) != STRSXP) {
    Rf_error("file_path must be a character string");
  }
  const char* file_path_c = CHAR(STRING_ELT(file_path, 0));
  FILE* file = fopen(file_path_c, "r");
  if (file == NULL) {
    Rf_error("Can not open file %s", file_path_c);
  }
  fclose(file);
  char* hash = sm3_hash_file(file_path_c);
  SEXP result = Rf_ScalarString(Rf_mkCharCE(hash, CE_UTF8));
  free_char_array(hash);
  return result;
}
