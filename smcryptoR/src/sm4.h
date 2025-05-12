#ifndef SM4_H
#define SM4_H

#include <Rinternals.h>

SEXP encrypt_ecb_wrapper(SEXP input_data, SEXP key);
SEXP encrypt_ecb_base64_wrapper(SEXP input_data, SEXP key);
SEXP encrypt_ecb_hex_wrapper(SEXP input_data, SEXP key);
SEXP encrypt_ecb_to_file_wrapper(SEXP input_file, SEXP output_file, SEXP key);
SEXP decrypt_ecb_wrapper(SEXP input_data, SEXP key);
SEXP decrypt_ecb_base64_wrapper(SEXP input_data, SEXP key);
SEXP decrypt_ecb_hex_wrapper(SEXP input_data, SEXP key);
SEXP decrypt_ecb_from_file_wrapper(SEXP input_file, SEXP output_file, SEXP key);
SEXP encrypt_cbc_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP encrypt_cbc_base64_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP encrypt_cbc_hex_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP encrypt_cbc_to_file_wrapper(SEXP input_file, SEXP output_file, SEXP key, SEXP iv);
SEXP decrypt_cbc_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP decrypt_cbc_base64_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP decrypt_cbc_hex_wrapper(SEXP input_data, SEXP key, SEXP iv);
SEXP decrypt_cbc_from_file_wrapper(SEXP input_file, SEXP output_file, SEXP key, SEXP iv);

#endif /* SM4_H */
