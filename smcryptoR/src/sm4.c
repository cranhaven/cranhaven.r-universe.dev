#include <string.h>
#include <Rinternals.h>
#include "smcrypto/api.h"
#include "sm4.h"

SEXP encrypt_ecb_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  size_t output_data_len;
  unsigned char* encrypt_data = encrypt_ecb(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), &output_data_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), encrypt_data, output_data_len);
  free_byte_array(encrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_ecb_base64_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  char* encrypt_str = encrypt_ecb_base64(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key));
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP encrypt_ecb_hex_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  char* encrypt_str = encrypt_ecb_hex(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key));
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP encrypt_ecb_to_file_wrapper(SEXP input_file, SEXP output_file, SEXP key) {
  if (TYPEOF(input_file) != STRSXP) {
    Rf_error("input_file must be a character string");
  }
  if (TYPEOF(output_file) != STRSXP) {
    Rf_error("output_file must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const char* input_file_c = CHAR(STRING_ELT(input_file, 0));
  const char* output_file_c = CHAR(STRING_ELT(output_file, 0));
  const unsigned char* key_c = RAW(key);
  FILE* file_i = fopen(input_file_c, "r");
  if (file_i == NULL) {
    Rf_error("Can not open file %s", input_file_c);
  }
  fclose(file_i);
  FILE* file_o = fopen(output_file_c, "w");
  if (file_o == NULL) {
    Rf_error("Can not open file %s", output_file_c);
  }
  fclose(file_o);
  encrypt_ecb_to_file(input_file_c, output_file_c, key_c, XLENGTH(key));
  return R_NilValue;
}

SEXP decrypt_ecb_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (XLENGTH(input_data) % 16 != 0) {
    Rf_error("Invalid input_data length");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_ecb(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_ecb_base64_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != STRSXP) {
    Rf_error("input_data must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const char* input_data_c = CHAR(STRING_ELT(input_data, 0));
  if (base64_valid(input_data_c) != 1) {
    Rf_error("input_data is not a valid base64 string");
  }
  const unsigned char* key_c = RAW(key);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_ecb_base64(input_data_c, key_c, XLENGTH(key), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_ecb_hex_wrapper(SEXP input_data, SEXP key) {
  if (TYPEOF(input_data) != STRSXP) {
    Rf_error("input_data must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const char* input_data_c = CHAR(STRING_ELT(input_data, 0));
  if (hex_valid(input_data_c) != 1) {
    Rf_error("input_data is not a valid hex string");
  }
  const unsigned char* key_c = RAW(key);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_ecb_hex(input_data_c, key_c, XLENGTH(key), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_ecb_from_file_wrapper(SEXP input_file, SEXP output_file, SEXP key) {
  if (TYPEOF(input_file) != STRSXP) {
    Rf_error("input_file must be a character string");
  }
  if (TYPEOF(output_file) != STRSXP) {
    Rf_error("output_file must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  const char* input_file_c = CHAR(STRING_ELT(input_file, 0));
  const char* output_file_c = CHAR(STRING_ELT(output_file, 0));
  const unsigned char* key_c = RAW(key);
  FILE* file_i = fopen(input_file_c, "r");
  if (file_i == NULL) {
    Rf_error("Can not open file %s", input_file_c);
  }
  fclose(file_i);
  FILE* file_o = fopen(output_file_c, "w");
  if (file_o == NULL) {
    Rf_error("Can not open file %s", output_file_c);
  }
  fclose(file_o);
  decrypt_ecb_from_file(input_file_c, output_file_c, key_c, XLENGTH(key));
  return R_NilValue;
}

SEXP encrypt_cbc_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  size_t output_data_len;
  unsigned char* encrypt_data = encrypt_cbc(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), iv_c, XLENGTH(iv), &output_data_len);
  if (output_data_len == 0) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), encrypt_data, output_data_len);
  free_byte_array(encrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_cbc_base64_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  char* encrypt_str = encrypt_cbc_base64(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), iv_c, XLENGTH(iv));
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP encrypt_cbc_hex_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  char* encrypt_str = encrypt_cbc_hex(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), iv_c, XLENGTH(iv));
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP encrypt_cbc_to_file_wrapper(SEXP input_file, SEXP output_file, SEXP key, SEXP iv) {
  if (TYPEOF(input_file) != STRSXP) {
    Rf_error("input_file must be a character string");
  }
  if (TYPEOF(output_file) != STRSXP) {
    Rf_error("output_file must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const char* input_file_c = CHAR(STRING_ELT(input_file, 0));
  const char* output_file_c = CHAR(STRING_ELT(output_file, 0));
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  FILE* file_i = fopen(input_file_c, "r");
  if (file_i == NULL) {
    Rf_error("Can not open file %s", input_file_c);
  }
  fclose(file_i);
  FILE* file_o = fopen(output_file_c, "w");
  if (file_o == NULL) {
    Rf_error("Can not open file %s", output_file_c);
  }
  fclose(file_o);
  encrypt_cbc_to_file(input_file_c, output_file_c, key_c, XLENGTH(key), iv_c, XLENGTH(iv));
  return R_NilValue;
}

SEXP decrypt_cbc_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != RAWSXP) {
    Rf_error("input_data must be a raw vector");
  }
  if (XLENGTH(input_data) % 16 != 0) {
    Rf_error("Invalid input_data length");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const unsigned char* input_data_c = RAW(input_data);
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_cbc(input_data_c, XLENGTH(input_data), key_c, XLENGTH(key), iv_c, XLENGTH(iv), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_cbc_base64_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != STRSXP) {
    Rf_error("input_data must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const char* input_data_c = CHAR(STRING_ELT(input_data, 0));
  if (base64_valid(input_data_c) != 1) {
    Rf_error("input_data is not a valid base64 string");
  }
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_cbc_base64(input_data_c, key_c, XLENGTH(key), iv_c, XLENGTH(iv), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_cbc_hex_wrapper(SEXP input_data, SEXP key, SEXP iv) {
  if (TYPEOF(input_data) != STRSXP) {
    Rf_error("input_data must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  const char* input_data_c = CHAR(STRING_ELT(input_data, 0));
  if (hex_valid(input_data_c) != 1) {
    Rf_error("input_data is not a valid hex string");
  }
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  size_t output_data_len;
  unsigned char* decrypt_data = decrypt_cbc_hex(input_data_c, key_c, XLENGTH(key), iv_c, XLENGTH(iv), &output_data_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, output_data_len));
  memcpy(RAW(result), decrypt_data, output_data_len);
  free_byte_array(decrypt_data, output_data_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_cbc_from_file_wrapper(SEXP input_file, SEXP output_file, SEXP key, SEXP iv) {
  if (TYPEOF(input_file) != STRSXP) {
    Rf_error("input_file must be a character string");
  }
  if (TYPEOF(output_file) != STRSXP) {
    Rf_error("output_file must be a character string");
  }
  if (TYPEOF(key) != RAWSXP) {
    Rf_error("key must be a raw vector");
  }
  if (TYPEOF(iv) != RAWSXP) {
    Rf_error("iv must be a raw vector");
  }
  const char* input_file_c = CHAR(STRING_ELT(input_file, 0));
  const char* output_file_c = CHAR(STRING_ELT(output_file, 0));
  const unsigned char* key_c = RAW(key);
  const unsigned char* iv_c = RAW(iv);
  if (XLENGTH(key) != 16) {
    Rf_error("The byte length of key must be 16");
  }
  if (XLENGTH(iv) != 16) {
    Rf_error("The byte length of iv must be 16");
  }
  FILE* file_i = fopen(input_file_c, "r");
  if (file_i == NULL) {
    Rf_error("Can not open file %s", input_file_c);
  }
  fclose(file_i);
  FILE* file_o = fopen(output_file_c, "w");
  if (file_o == NULL) {
    Rf_error("Can not open file %s", output_file_c);
  }
  fclose(file_o);
  decrypt_cbc_from_file(input_file_c, output_file_c, key_c, XLENGTH(key), iv_c, XLENGTH(iv));
  return R_NilValue;
}
