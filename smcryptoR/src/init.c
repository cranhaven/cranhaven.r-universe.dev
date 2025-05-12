#include "sm3.h"
#include "sm2.h"
#include "sm4.h"

static const R_CallMethodDef CallEntries[] = {
  {"sm3_hash_wrapper", (DL_FUNC) &sm3_hash_wrapper, 1},
  {"sm3_hash_string_wrapper", (DL_FUNC) &sm3_hash_string_wrapper, 1},
  {"sm3_hash_file_wrapper", (DL_FUNC) &sm3_hash_file_wrapper, 1},
  {"gen_keypair_wrapper", (DL_FUNC) &gen_keypair_wrapper, 0},
  {"pk_from_sk_wrapper", (DL_FUNC) &pk_from_sk_wrapper, 1},
  {"privkey_valid_wrapper", (DL_FUNC) &privkey_valid_wrapper, 1},
  {"pubkey_valid_wrapper", (DL_FUNC) &pubkey_valid_wrapper, 1},
  {"keypair_from_pem_file_wrapper", (DL_FUNC) &keypair_from_pem_file_wrapper, 1},
  {"keypair_to_pem_file_wrapper", (DL_FUNC) &keypair_to_pem_file_wrapper, 2},
  {"pubkey_from_pem_file_wrapper", (DL_FUNC) &pubkey_from_pem_file_wrapper, 1},
  {"pubkey_to_pem_file_wrapper", (DL_FUNC) &pubkey_to_pem_file_wrapper, 2},
  {"sign_wrapper", (DL_FUNC) &sign_wrapper, 3},
  {"verify_wrapper", (DL_FUNC) &verify_wrapper, 4},
  {"sign_to_file_wrapper", (DL_FUNC) &sign_to_file_wrapper, 4},
  {"verify_from_file_wrapper", (DL_FUNC) &verify_from_file_wrapper, 4},
  {"keyexchange_1ab_wrapper", (DL_FUNC) &keyexchange_1ab_wrapper, 3},
  {"keyexchange_2a_wrapper", (DL_FUNC) &keyexchange_2a_wrapper, 4},
  {"keyexchange_2b_wrapper", (DL_FUNC) &keyexchange_2b_wrapper, 4},
  {"encrypt_wrapper", (DL_FUNC) &encrypt_wrapper, 2},
  {"decrypt_wrapper", (DL_FUNC) &decrypt_wrapper, 2},
  {"encrypt_c1c2c3_wrapper", (DL_FUNC) &encrypt_c1c2c3_wrapper, 2},
  {"decrypt_c1c2c3_wrapper", (DL_FUNC) &decrypt_c1c2c3_wrapper, 2},
  {"encrypt_asna1_wrapper", (DL_FUNC) &encrypt_asna1_wrapper, 2},
  {"decrypt_asna1_wrapper", (DL_FUNC) &decrypt_asna1_wrapper, 2},
  {"encrypt_hex_wrapper", (DL_FUNC) &encrypt_hex_wrapper, 2},
  {"decrypt_hex_wrapper", (DL_FUNC) &decrypt_hex_wrapper, 2},
  {"encrypt_base64_wrapper", (DL_FUNC) &encrypt_base64_wrapper, 2},
  {"decrypt_base64_wrapper", (DL_FUNC) &decrypt_base64_wrapper, 2},
  {"encrypt_to_file_wrapper", (DL_FUNC) &encrypt_to_file_wrapper, 3},
  {"decrypt_from_file_wrapper", (DL_FUNC) &decrypt_from_file_wrapper, 2},
  {"encrypt_ecb_wrapper", (DL_FUNC) &encrypt_ecb_wrapper, 2},
  {"encrypt_ecb_base64_wrapper", (DL_FUNC) &encrypt_ecb_base64_wrapper, 2},
  {"encrypt_ecb_hex_wrapper", (DL_FUNC) &encrypt_ecb_hex_wrapper, 2},
  {"encrypt_ecb_to_file_wrapper", (DL_FUNC) &encrypt_ecb_to_file_wrapper, 3},
  {"decrypt_ecb_wrapper", (DL_FUNC) &decrypt_ecb_wrapper, 2},
  {"decrypt_ecb_base64_wrapper", (DL_FUNC) &decrypt_ecb_base64_wrapper, 2},
  {"decrypt_ecb_hex_wrapper", (DL_FUNC) &decrypt_ecb_hex_wrapper, 2},
  {"decrypt_ecb_from_file_wrapper", (DL_FUNC) &decrypt_ecb_from_file_wrapper, 3},
  {"encrypt_cbc_wrapper", (DL_FUNC) &encrypt_cbc_wrapper, 3},
  {"encrypt_cbc_base64_wrapper", (DL_FUNC) &encrypt_cbc_base64_wrapper, 3},
  {"encrypt_cbc_hex_wrapper", (DL_FUNC) &encrypt_cbc_hex_wrapper, 3},
  {"encrypt_cbc_to_file_wrapper", (DL_FUNC) &encrypt_cbc_to_file_wrapper, 4},
  {"decrypt_cbc_wrapper", (DL_FUNC) &decrypt_cbc_wrapper, 3},
  {"decrypt_cbc_base64_wrapper", (DL_FUNC) &decrypt_cbc_base64_wrapper, 3},
  {"decrypt_cbc_hex_wrapper", (DL_FUNC) &decrypt_cbc_hex_wrapper, 3},
  {"decrypt_cbc_from_file_wrapper", (DL_FUNC) &decrypt_cbc_from_file_wrapper, 4},
  {NULL, NULL, 0}
};

void R_init_smcryptoR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
