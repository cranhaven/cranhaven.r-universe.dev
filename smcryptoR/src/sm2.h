#ifndef SM2_H
#define SM2_H

#include <Rinternals.h>

SEXP gen_keypair_wrapper(void);
SEXP pk_from_sk_wrapper(SEXP private_key);
SEXP privkey_valid_wrapper(SEXP private_key);
SEXP pubkey_valid_wrapper(SEXP public_key);
SEXP keypair_from_pem_file_wrapper(SEXP pem_file);
SEXP keypair_to_pem_file_wrapper(SEXP private_key, SEXP pem_file);
SEXP pubkey_from_pem_file_wrapper(SEXP pem_file);
SEXP pubkey_to_pem_file_wrapper(SEXP public_key, SEXP pem_file);
SEXP sign_wrapper(SEXP id, SEXP data, SEXP private_key);
SEXP verify_wrapper(SEXP id, SEXP data, SEXP sign, SEXP public_key);
SEXP sign_to_file_wrapper(SEXP id, SEXP data, SEXP sign_file, SEXP private_key);
SEXP verify_from_file_wrapper(SEXP id, SEXP data, SEXP sign_file, SEXP public_key);
SEXP keyexchange_1ab_wrapper(SEXP klen, SEXP id, SEXP private_key);
SEXP keyexchange_2a_wrapper(SEXP id, SEXP private_key, SEXP private_key_r, SEXP recive_bytes);
SEXP keyexchange_2b_wrapper(SEXP id, SEXP private_key, SEXP private_key_r, SEXP recive_bytes);
SEXP encrypt_wrapper(SEXP data, SEXP public_key);
SEXP decrypt_wrapper(SEXP data, SEXP private_key);
SEXP encrypt_c1c2c3_wrapper(SEXP data, SEXP public_key);
SEXP decrypt_c1c2c3_wrapper(SEXP data, SEXP private_key);
SEXP encrypt_asna1_wrapper(SEXP data, SEXP public_key);
SEXP decrypt_asna1_wrapper(SEXP data, SEXP private_key);
SEXP encrypt_hex_wrapper(SEXP data, SEXP public_key);
SEXP decrypt_hex_wrapper(SEXP data, SEXP private_key);
SEXP encrypt_base64_wrapper(SEXP data, SEXP public_key);
SEXP decrypt_base64_wrapper(SEXP data, SEXP private_key);
SEXP encrypt_to_file_wrapper(SEXP data, SEXP enc_file, SEXP public_key);
SEXP decrypt_from_file_wrapper(SEXP dec_file, SEXP private_key);

#endif /* SM2_H */
