#include <string.h>
#include <Rinternals.h>
#include "smcrypto/api.h"
#include "sm2.h"

SEXP gen_keypair_wrapper(void) {
  Keypair* keypair = gen_keypair();
  SEXP r_list = PROTECT(allocVector(VECSXP, 2));
  SEXP r_name = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r_list, 0, mkString(keypair->private_key));
  SET_VECTOR_ELT(r_list, 1, mkString(keypair->public_key));
  SET_STRING_ELT(r_name, 0, mkChar("private_key"));
  SET_STRING_ELT(r_name, 1, mkChar("public_key"));
  setAttrib(r_list, R_NamesSymbol, r_name);
  free_struct_keypair(keypair);
  UNPROTECT(2);
  return r_list;
}

SEXP pk_from_sk_wrapper(SEXP private_key) {
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  char* public_key = pk_from_sk(private_key_c);
  SEXP result = Rf_ScalarString(Rf_mkCharCE(public_key, CE_UTF8));
  free_char_array(public_key);
  return result;
}

SEXP privkey_valid_wrapper(SEXP private_key) {
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  int valid = privkey_valid(private_key_c);
  SEXP result = ScalarInteger(valid);
  return result;
}

SEXP pubkey_valid_wrapper(SEXP public_key) {
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  int valid = pubkey_valid(public_key_c);
  SEXP result = ScalarInteger(valid);
  return result;
}

SEXP keypair_from_pem_file_wrapper(SEXP pem_file) {
  if (TYPEOF(pem_file) != STRSXP) {
    Rf_error("pem_file must be a character string");
  }
  const char* pem_file_c = CHAR(STRING_ELT(pem_file, 0));
  FILE* file = fopen(pem_file_c, "r");
  if (file == NULL) {
    Rf_error("Can not open file %s", pem_file_c);
  }
  fclose(file);
  Keypair* keypair = keypair_from_pem_file(pem_file_c);
  if (strcmp(keypair->private_key, "") == 0 || strcmp(keypair->public_key, "") == 0) {
    Rf_error("Invalid keypair");
  }
  SEXP r_list = PROTECT(allocVector(VECSXP, 2));
  SEXP r_name = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r_list, 0, mkString(keypair->private_key));
  SET_VECTOR_ELT(r_list, 1, mkString(keypair->public_key));
  SET_STRING_ELT(r_name, 0, mkChar("private_key"));
  SET_STRING_ELT(r_name, 1, mkChar("public_key"));
  setAttrib(r_list, R_NamesSymbol, r_name);
  free_struct_keypair(keypair);
  UNPROTECT(2);
  return r_list;
}

SEXP keypair_to_pem_file_wrapper(SEXP private_key, SEXP pem_file) {
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  if (TYPEOF(pem_file) != STRSXP) {
    Rf_error("pem_file must be a character string");
  }
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  const char* pem_file_c = CHAR(STRING_ELT(pem_file, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  FILE* file = fopen(pem_file_c, "w");
  if (file == NULL) {
    Rf_error("Can not open file %s", pem_file_c);
  }
  fclose(file);
  keypair_to_pem_file(private_key_c, pem_file_c);
  return R_NilValue;
}

SEXP pubkey_from_pem_file_wrapper(SEXP pem_file) {
  if (TYPEOF(pem_file) != STRSXP) {
    Rf_error("pem_file must be a character string");
  }
  const char* pem_file_c = CHAR(STRING_ELT(pem_file, 0));
  char* public_key_c = pubkey_from_pem_file(pem_file_c);
  if (strcmp(public_key_c, "") == 0) {
    Rf_error("Invalid public key");
  }
  FILE* file = fopen(pem_file_c, "r");
  if (file == NULL) {
    Rf_error("Can not open file %s", pem_file_c);
  }
  fclose(file);
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  SEXP result = Rf_ScalarString(Rf_mkCharCE(public_key_c, CE_UTF8));
  free_char_array(public_key_c);
  return result;
}

SEXP pubkey_to_pem_file_wrapper(SEXP public_key, SEXP pem_file) {
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  if (TYPEOF(pem_file) != STRSXP) {
    Rf_error("pem_file must be a character string");
  }
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  const char* pem_file_c = CHAR(STRING_ELT(pem_file, 0));
  FILE* file = fopen(pem_file_c, "w");
  if (file == NULL) {
    Rf_error("Can not open file %s", pem_file_c);
  }
  fclose(file);
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  pubkey_to_pem_file(public_key_c, pem_file_c);
  return R_NilValue;
}

SEXP sign_wrapper(SEXP id, SEXP data, SEXP private_key) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const unsigned char* id_raw = RAW(id);
  const unsigned char* data_raw = RAW(data);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  uintptr_t sig_len = 0;
  unsigned char* sign_data = sign(id_raw, XLENGTH(id), data_raw, XLENGTH(data), private_key_c, &sig_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, sig_len));
  memcpy(RAW(result), sign_data, sig_len);
  free_byte_array(sign_data, sig_len);
  UNPROTECT(1);
  return result;
}

SEXP verify_wrapper(SEXP id, SEXP data, SEXP sign, SEXP public_key) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(sign) != RAWSXP) {
    Rf_error("sign must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* id_raw = RAW(id);
  const unsigned char* data_raw = RAW(data);
  const unsigned char* sign_raw = RAW(sign);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  int verify_result = verify(id_raw, XLENGTH(id), data_raw, XLENGTH(data), sign_raw, XLENGTH(sign), public_key_c);
  SEXP result = ScalarInteger(verify_result);
  return result;
}

SEXP sign_to_file_wrapper(SEXP id, SEXP data, SEXP sign_file, SEXP private_key) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(sign_file) != STRSXP) {
    Rf_error("sign_file must be a character string");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const unsigned char* id_raw = RAW(id);
  const unsigned char* data_raw = RAW(data);
  const char* sign_file_c = CHAR(STRING_ELT(sign_file, 0));
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  FILE* file = fopen(sign_file_c, "w");
  if (file == NULL) {
    Rf_error("Can not open file %s", sign_file_c);
  }
  fclose(file);
  sign_to_file(id_raw, XLENGTH(id), data_raw, XLENGTH(data), sign_file_c, private_key_c);
  return R_NilValue;
}

SEXP verify_from_file_wrapper(SEXP id, SEXP data, SEXP sign_file, SEXP public_key) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(sign_file) != STRSXP) {
    Rf_error("sign_file must be a character string");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* id_raw = RAW(id);
  const unsigned char* data_raw = RAW(data);
  const char* sign_file_c = CHAR(STRING_ELT(sign_file, 0));
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  FILE* file = fopen(sign_file_c, "r");
  if (file == NULL) {
    Rf_error("Can not open file %s", sign_file_c);
  }
  fclose(file);
  int verify_result = verify_from_file(id_raw, XLENGTH(id), data_raw, XLENGTH(data), sign_file_c, public_key_c);
  SEXP result = ScalarInteger(verify_result);
  return result;
}

SEXP keyexchange_1ab_wrapper(SEXP klen, SEXP id, SEXP private_key) {
  if (TYPEOF(klen) != INTSXP) {
    Rf_error("klen must be an integer");
  }
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  size_t klen_c = INTEGER(klen)[0];
  const unsigned char* id_raw = RAW(id);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t data_len = 0;
  KeyExchangeData* keyexchangedata = keyexchange_1ab(klen_c, id_raw, XLENGTH(id), private_key_c, &data_len);
  SEXP r_list = PROTECT(allocVector(VECSXP, 2));
  SEXP r_name = PROTECT(allocVector(STRSXP, 2));
  SEXP keyexchangedata_data = PROTECT(Rf_allocVector(RAWSXP, data_len));
  memcpy(RAW(keyexchangedata_data), keyexchangedata->data, data_len);
  SET_VECTOR_ELT(r_list, 0, keyexchangedata_data);
  SET_VECTOR_ELT(r_list, 1, mkString(keyexchangedata->private_key_r));
  SET_STRING_ELT(r_name, 0, mkChar("data"));
  SET_STRING_ELT(r_name, 1, mkChar("private_key_r"));
  setAttrib(r_list, R_NamesSymbol, r_name);
  free_struct_keyexchangedata(keyexchangedata);
  UNPROTECT(3);
  return r_list;
}

SEXP keyexchange_2a_wrapper(SEXP id, SEXP private_key, SEXP private_key_r, SEXP recive_bytes) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  if (TYPEOF(private_key_r) != STRSXP) {
    Rf_error("private_key_r must be a character string");
  }
  if (TYPEOF(recive_bytes) != RAWSXP) {
    Rf_error("recive_bytes must be a raw vector");
  }
  const unsigned char* id_raw = RAW(id);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  const char* private_key_r_c = CHAR(STRING_ELT(private_key_r, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  if (privkey_valid(private_key_r_c) != 1) {
    Rf_error("Invalid private key");
  }
  const unsigned char* recive_bytes_raw = RAW(recive_bytes);
  size_t s12_len = 0;
  KeyExchangeResult* keyexchangeresult = keyexchange_2a(id_raw, XLENGTH(id), private_key_c, private_key_r_c, recive_bytes_raw, XLENGTH(recive_bytes), &s12_len);
  if (strcmp(keyexchangeresult->k, "") == 0 || s12_len == 0) {
    Rf_error("key exchange failed");
  }
  SEXP r_list = PROTECT(allocVector(VECSXP, 2));
  SEXP r_name = PROTECT(allocVector(STRSXP, 2));
  SEXP keyexchangeresult_s12 = PROTECT(Rf_allocVector(RAWSXP, s12_len));
  memcpy(RAW(keyexchangeresult_s12), keyexchangeresult->s12, s12_len);
  SET_VECTOR_ELT(r_list, 0, mkString(keyexchangeresult->k));
  SET_VECTOR_ELT(r_list, 1, keyexchangeresult_s12);
  SET_STRING_ELT(r_name, 0, mkChar("k"));
  SET_STRING_ELT(r_name, 1, mkChar("s12"));
  setAttrib(r_list, R_NamesSymbol, r_name);
  free_struct_keyexchangeresult(keyexchangeresult);
  UNPROTECT(3);
  return r_list;
}

SEXP keyexchange_2b_wrapper(SEXP id, SEXP private_key, SEXP private_key_r, SEXP recive_bytes) {
  if (TYPEOF(id) != RAWSXP) {
    Rf_error("id must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  if (TYPEOF(private_key_r) != STRSXP) {
    Rf_error("private_key_r must be a character string");
  }
  if (TYPEOF(recive_bytes) != RAWSXP) {
    Rf_error("recive_bytes must be a raw vector");
  }
  const unsigned char* id_raw = RAW(id);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  const char* private_key_r_c = CHAR(STRING_ELT(private_key_r, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  if (privkey_valid(private_key_r_c) != 1) {
    Rf_error("Invalid private key");
  }
  const unsigned char* recive_bytes_raw = RAW(recive_bytes);
  size_t s12_len = 0;
  KeyExchangeResult* keyexchangeresult = keyexchange_2b(id_raw, XLENGTH(id), private_key_c, private_key_r_c, recive_bytes_raw, XLENGTH(recive_bytes), &s12_len);
  if (strcmp(keyexchangeresult->k, "") == 0 || s12_len == 0) {
    Rf_error("key exchange failed");
  }
  SEXP r_list = PROTECT(allocVector(VECSXP, 2));
  SEXP r_name = PROTECT(allocVector(STRSXP, 2));
  SEXP keyexchangeresult_s12 = PROTECT(Rf_allocVector(RAWSXP, s12_len));
  memcpy(RAW(keyexchangeresult_s12), keyexchangeresult->s12, s12_len);
  SET_VECTOR_ELT(r_list, 0, mkString(keyexchangeresult->k));
  SET_VECTOR_ELT(r_list, 1, keyexchangeresult_s12);
  SET_STRING_ELT(r_name, 0, mkChar("k"));
  SET_STRING_ELT(r_name, 1, mkChar("s12"));
  setAttrib(r_list, R_NamesSymbol, r_name);
  free_struct_keyexchangeresult(keyexchangeresult);
  UNPROTECT(3);
  return r_list;
}

SEXP encrypt_wrapper(SEXP data, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  size_t enc_len;
  unsigned char* encrypt_data = encrypt(data_raw, XLENGTH(data), public_key_c, &enc_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, enc_len));
  memcpy(RAW(result), encrypt_data, enc_len);
  free_byte_array(encrypt_data, enc_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_wrapper(SEXP data, SEXP private_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt(data_raw, XLENGTH(data), private_key_c, &dec_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_c1c2c3_wrapper(SEXP data, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  size_t enc_len;
  unsigned char* encrypt_data = encrypt_c1c2c3(data_raw, XLENGTH(data), public_key_c, &enc_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, enc_len));
  memcpy(RAW(result), encrypt_data, enc_len);
  free_byte_array(encrypt_data, enc_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_c1c2c3_wrapper(SEXP data, SEXP private_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt_c1c2c3(data_raw, XLENGTH(data), private_key_c, &dec_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_asna1_wrapper(SEXP data, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  size_t enc_len;
  unsigned char* encrypt_data = encrypt_asna1(data_raw, XLENGTH(data), public_key_c, &enc_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, enc_len));
  memcpy(RAW(result), encrypt_data, enc_len);
  free_byte_array(encrypt_data, enc_len);
  UNPROTECT(1);
  return result;
}

SEXP decrypt_asna1_wrapper(SEXP data, SEXP private_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt_asna1(data_raw, XLENGTH(data), private_key_c, &dec_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_hex_wrapper(SEXP data, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  char* encrypt_str = encrypt_hex(data_raw, XLENGTH(data), public_key_c);
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP decrypt_hex_wrapper(SEXP data, SEXP private_key) {
  if (TYPEOF(data) != STRSXP) {
    Rf_error("data must be a character string");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const char* data_c = CHAR(STRING_ELT(data, 0));
  if (hex_valid(data_c) != 1) {
    Rf_error("data is not a valid hex string");
  }
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt_hex(data_c, private_key_c, &dec_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_base64_wrapper(SEXP data, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  char* encrypt_str = encrypt_base64(data_raw, XLENGTH(data), public_key_c);
  SEXP result = Rf_ScalarString(Rf_mkCharCE(encrypt_str, CE_UTF8));
  free_char_array(encrypt_str);
  return result;
}

SEXP decrypt_base64_wrapper(SEXP data, SEXP private_key) {
  if (TYPEOF(data) != STRSXP) {
    Rf_error("data must be a character string");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const char* data_c = CHAR(STRING_ELT(data, 0));
  if (base64_valid(data_c) != 1) {
    Rf_error("data is not a valid base64 string");
  }
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  if (base64_valid(data_c) != 1) {
    Rf_error("Invalid base64 input");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt_base64(data_c, private_key_c, &dec_len);
  if (decrypt_data == NULL) {
    Rf_error("decrypt failed");
  }
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}

SEXP encrypt_to_file_wrapper(SEXP data, SEXP enc_file, SEXP public_key) {
  if (TYPEOF(data) != RAWSXP) {
    Rf_error("data must be a raw vector");
  }
  if (TYPEOF(enc_file) != STRSXP) {
    Rf_error("enc_file must be a character string");
  }
  if (TYPEOF(public_key) != STRSXP) {
    Rf_error("public_key must be a character string");
  }
  const unsigned char* data_raw = RAW(data);
  const char* enc_file_c = CHAR(STRING_ELT(enc_file, 0));
  const char* public_key_c = CHAR(STRING_ELT(public_key, 0));
  if (pubkey_valid(public_key_c) != 1) {
    Rf_error("Invalid public key");
  }
  FILE* file = fopen(enc_file_c, "w");
  if (file == NULL) {
    Rf_error("Can not open file %s", enc_file_c);
  }
  fclose(file);
  encrypt_to_file(data_raw, XLENGTH(data), enc_file_c, public_key_c);
  return R_NilValue;
}

SEXP decrypt_from_file_wrapper(SEXP dec_file, SEXP private_key) {
  if (TYPEOF(dec_file) != STRSXP) {
    Rf_error("dec_file must be a character string");
  }
  if (TYPEOF(private_key) != STRSXP) {
    Rf_error("private_key must be a character string");
  }
  const char* dec_file_c = CHAR(STRING_ELT(dec_file, 0));
  const char* private_key_c = CHAR(STRING_ELT(private_key, 0));
  FILE* file = fopen(dec_file_c, "r");
  if (file == NULL) {
    Rf_error("Can not open file %s", dec_file_c);
  }
  fclose(file);
  if (privkey_valid(private_key_c) != 1) {
    Rf_error("Invalid private key");
  }
  size_t dec_len;
  unsigned char* decrypt_data = decrypt_from_file(dec_file_c, private_key_c, &dec_len);
  SEXP result = PROTECT(Rf_allocVector(RAWSXP, dec_len));
  memcpy(RAW(result), decrypt_data, dec_len);
  free_byte_array(decrypt_data, dec_len);
  UNPROTECT(1);
  return result;
}
