#' @title SM2 Key Pair
#' @description
#' In the SM2 encryption algorithm, the private key and public key appear in pairs.
#' The private key is a 64-bit hexadecimal string, and the public key is a
#' 128-bit hexadecimal string, excluding the "04" prefix at the beginning.
#' The public key is included in the private key and can be derived from the
#' private key. We use the public key for encryption, the private key for decryption,
#' the private key for signing, and the public key for verification.
#' @details
#' \describe{
#'   \item{\link{sm2_gen_keypair}}{generate a ramdom key pair}
#'   \item{\link{sm2_pk_from_sk}}{export public key from a private key}
#'   \item{\link{sm2_privkey_valid}}{check whether a private key is legal}
#'   \item{\link{sm2_pubkey_valid}}{check whether a public key is legal}
#'   \item{\link{sm2_keypair_from_pem_file}}{import private key from a local pem file}
#'   \item{\link{sm2_keypair_to_pem_file}}{save a private key to a local pem file}
#'   \item{\link{sm2_pubkey_from_pem_file}}{import public key from a local pem file}
#'   \item{\link{sm2_pubkey_to_pem_file}}{save a public key to a local pem file}
#' }
#' @rdname sm2_keypair
#' @return 
#' \describe{
#'   \item{\link{sm2_gen_keypair}}{returns a list contains a random private key and the corresponding public key}
#'   \item{\link{sm2_pk_from_sk}}{returns a character string, the public key exported from a private key}
#'   \item{\link{sm2_privkey_valid}}{returns 1 if valid, 0 if invalid}
#'   \item{\link{sm2_pubkey_valid}}{returns 1 if valid, 0 if invalid}
#'   \item{\link{sm2_keypair_from_pem_file}}{returns a list contains a random private key and the corresponding public key}
#'   \item{\link{sm2_keypair_to_pem_file}}{returns nothing, and a local file contains the keypair will be saved in the specified path}
#'   \item{\link{sm2_pubkey_from_pem_file}}{returns a character string, the public key saved in the local file}
#'   \item{\link{sm2_pubkey_to_pem_file}}{returns nothing, and a local file contains the public key will be saved in the specified path}
#' }
#' @examples
#' ## generate a ramdom keypair
#' keypair <- sm2_gen_keypair()
#' keypair$private_key
#' keypair$public_key
#' ## export public key from private key
#' sm2_pk_from_sk(keypair$private_key)
#' ## check whether the private key is legal
#' sm2_privkey_valid(keypair$private_key)
#' ## check whether the public key is legal
#' sm2_pubkey_valid(keypair$public_key)
#' \dontrun{
#'   sm2_keypair_to_pem_file(keypair, 'keypair.pem')
#'   sm2_keypair_from_pem_file('keypair.pem')
#'   sm2_pubkey_to_pem_file(keypair$public_key, 'pubkey.pem')
#'   sm2_pubkey_from_pem_file('pubkey.pem')
#' }
#' @export
#' @useDynLib smcryptoR gen_keypair_wrapper
sm2_gen_keypair <- function() {
  .Call(gen_keypair_wrapper)
}

#' @rdname sm2_keypair
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR pk_from_sk_wrapper
sm2_pk_from_sk <- function(private_key) {
  .Call(pk_from_sk_wrapper, private_key)
}

#' @rdname sm2_keypair
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR privkey_valid_wrapper
sm2_privkey_valid <- function(private_key) {
  .Call(privkey_valid_wrapper, private_key)
}

#' @rdname sm2_keypair
#' @param public_key a public key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR pubkey_valid_wrapper
sm2_pubkey_valid <- function(public_key) {
  .Call(pubkey_valid_wrapper, public_key)
}

#' @rdname sm2_keypair
#' @param pem_file local pem file path
#' @export
#' @useDynLib smcryptoR keypair_from_pem_file_wrapper
sm2_keypair_from_pem_file <- function(pem_file) {
  .Call(keypair_from_pem_file_wrapper, pem_file)
}

#' @rdname sm2_keypair
#' @param private_key a private key represented as a hexadecimal string
#' @param pem_file local pem file path
#' @export
#' @useDynLib smcryptoR keypair_to_pem_file_wrapper
sm2_keypair_to_pem_file <- function(private_key, pem_file) {
  .Call(keypair_to_pem_file_wrapper, private_key, pem_file)
  invisible()
}

#' @rdname sm2_keypair
#' @param pem_file local pem file path
#' @export
#' @useDynLib smcryptoR pubkey_from_pem_file_wrapper
sm2_pubkey_from_pem_file <- function(pem_file) {
  .Call(pubkey_from_pem_file_wrapper, pem_file)
}

#' @rdname sm2_keypair
#' @param public_key a public key represented as a hexadecimal string
#' @param pem_file local pem file path
#' @export
#' @useDynLib smcryptoR pubkey_to_pem_file_wrapper
sm2_pubkey_to_pem_file <- function(public_key, pem_file) {
  .Call(pubkey_to_pem_file_wrapper, public_key, pem_file)
  invisible()
}

#' @title SM2 Sign/Verify
#' @description
#' SM2 is an asymmetric encryption algorithm, so it can be used to sign and verify
#' signatures of data. The purpose of doing this is to ensure the integrity of
#' the data and guarantee its authenticity. Typically, the data owner uses the
#' SM3 message digest algorithm to calculate the hash value and signs it with the
#' private key, generating signed data. Then the owner distributes the original
#' data and the signed data of the original data to the receiver. The receiver
#' uses the public key and the received signed data to perform the verification
#' operation. If the verification is successful, it is considered that the
#' received original data has not been tampered with.
#' @rdname sm2_sign
#' @param id the signer's id, must be a raw vector
#' @param data orignal data, must be a raw vector
#' @param private_key a private key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_sign}}{returns a raw vector contains the signature}
#'   \item{\link{sm2_verify}}{returns 1 if verified, 0 if not verified}
#'   \item{\link{sm2_sign_to_file}}{returns nothing, and a signature file will be saved in the specified path}
#'   \item{\link{sm2_verify_from_file}}{returns 1 if verified, 0 if not verified}
#' }
#' @examples
#' ## sign and verify
#' id <- charToRaw('yumeng@company.com')
#' data <- charToRaw('abc')
#' keypair <- sm2_gen_keypair()
#' private_key <- keypair$private_key
#' public_key <- keypair$public_key
#' sign_data <- sm2_sign(id, data, private_key)
#' verify_result <- sm2_verify(id, data, sign_data, public_key)
#' \dontrun{
#'   sm2_sign_to_file(id, data, 'sign_data.sig', private_key)
#'   sm2_verify_from_file(id, data, 'sign_data.sig', public_key)
#' }
#' @export
#' @useDynLib smcryptoR sign_wrapper
sm2_sign <- function(id, data, private_key) {
  .Call(sign_wrapper, id, data, private_key)
}

#' @rdname sm2_sign
#' @param id the signer's id, must be a raw vector
#' @param data orignal data, must be a raw vector
#' @param sign sign data of the original data or file
#' @param public_key a public key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR verify_wrapper
sm2_verify <- function(id, data, sign, public_key) {
  .Call(verify_wrapper, id, data, sign, public_key)
}

#' @rdname sm2_sign
#' @param id the signer's id, must be a raw vector
#' @param data orignal data, must be a raw vector
#' @param sign_file file path of the sign data to save
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR sign_to_file_wrapper
sm2_sign_to_file <- function(id, data, sign_file, private_key) {
  .Call(sign_to_file_wrapper, id, data, sign_file, private_key);
  invisible()
}

#' @rdname sm2_sign
#' @param id the signer's id, must be a raw vector
#' @param data orignal data, must be a raw vector
#' @param sign_file file path of the sign data to load
#' @param public_key a public key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR verify_from_file_wrapper
sm2_verify_from_file <- function(id, data, sign_file, public_key) {
  .Call(verify_from_file_wrapper, id, data, sign_file, public_key)
}

#' @title SM2 Key Exchange
#' @description
#' SM2 is an asymmetric encryption algorithm, therefore, it can also be used for
#' key agreement or key exchange. If A and B want to generate a recognized key
#' for encryption or authentication, this algorithm can ensure that the key
#' itself will not be transmitted through untrusted channels, and the private
#' keys of A and B will not be disclosed. Even if an attacker intercepts the
#' data exchanged by A and B, they cannot calculate the key agreed upon by A and B.
#' @rdname sm2_keyexchange
#' @param klen the key length, must be an integer
#' @param id id of A or B, must be a raw vector
#' @param private_key private key of A or B represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_keyexchange_1ab}}{returns a list, `data` for the raw data sent to B(for A) or A(for B), `private_key_r` for the temporary private key}
#'   \item{\link{sm2_keyexchange_2a}}{returns a list, `k` for the key of length `klen`, `s12` for the sm3 hash in asn.1 encoding}
#'   \item{\link{sm2_keyexchange_2b}}{returns a list, `k` for the key of length `klen`, `s12` for the sm3 hash in asn.1 encoding}
#' }
#' @examples
#' ## Step 1
#' klen <- 16
#' id_a <- "a@company.com" |> charToRaw()
#' id_b <- "b@company.com" |> charToRaw()
#' private_key_a <- sm2_gen_keypair()$private_key
#' private_key_b <- sm2_gen_keypair()$private_key
#' step_1_a <- sm2_keyexchange_1ab(klen, id_a, private_key_a)
#' step_1_b <- sm2_keyexchange_1ab(klen, id_b, private_key_b)
#'
#' ## Step 2
#' step_2_a <- sm2_keyexchange_2a(id_a, private_key_a, step_1_a$private_key_r, step_1_b$data)
#' step_2_b <- sm2_keyexchange_2b(id_b, private_key_b, step_1_b$private_key_r, step_1_a$data)
#' step_2_a$k
#' step_2_b$k
#' @export
#' @useDynLib smcryptoR keyexchange_1ab_wrapper
sm2_keyexchange_1ab <- function(klen, id, private_key) {
  klen <- as.integer(klen)
  .Call(keyexchange_1ab_wrapper, klen, id, private_key)
}

#' @rdname sm2_keyexchange
#' @param id id of A or B, must be a raw vector
#' @param private_key private key of A or B represented as a hexadecimal string
#' @param private_key_r temp private_key of A or B
#' @param recive_bytes for A or B, the recived data from B or A
#' @export
#' @useDynLib smcryptoR keyexchange_2a_wrapper
sm2_keyexchange_2a <- function(id, private_key, private_key_r, recive_bytes) {
  .Call(keyexchange_2a_wrapper, id, private_key, private_key_r, recive_bytes)
}

#' @rdname sm2_keyexchange
#' @param id id of A or B, must be a raw vector
#' @param private_key private key of A or B represented as a hexadecimal string
#' @param private_key_r temp private_key of A or B
#' @param recive_bytes for A or B, the recived data from B or A
#' @export
#' @useDynLib smcryptoR keyexchange_2b_wrapper
sm2_keyexchange_2b <- function(id, private_key, private_key_r, recive_bytes) {
  .Call(keyexchange_2b_wrapper, id, private_key, private_key_r, recive_bytes)
}

#' @title SM2 Encrypt/Decrypt
#' @description
#' SM2 is an asymmetric encryption algorithm that can also be used to directly
#' encrypt data. Typically, A encrypts a file or data using the public key,
#' passes the ciphertext to B, and B decrypts it using the corresponding private key.
#' SM2 encryption and decryption are suitable for shorter texts only.
#' For larger files, the process can be very slow. According to the SM2 algorithm
#' usage specifications, the encrypted ciphertext needs to be ASN.1 encoded.
#' We provide the functions sm2_encrypt_asna1 and sm2_decrypt_asna1 for this purpose.
#' Additionally, some scenarios use different arrangements of c1, c2, c3,
#' so we also offer the functions sm2_encrypt_c1c2c3 and sm2_decrypt_c1c2c3.
#' To facilitate the transmission of binary data, we also provide functions
#' to encrypt data into hexadecimal or base64 strings and decrypt from them.
#' @rdname sm2_encrypt
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param public_key a public key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_encrypt}}{returns a raw vector of the cipher text}
#'   \item{\link{sm2_decrypt}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## encrypt and decrypt - raw
#' keypair <- sm2_gen_keypair()
#' private_key <- keypair$private_key
#' public_key <- keypair$public_key
#' data <- 'abc' |> charToRaw()
#' enc <- sm2_encrypt(data, public_key)
#' enc
#' dec <- sm2_decrypt(enc, private_key)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_wrapper
sm2_encrypt <- function(data, public_key) {
  .Call(encrypt_wrapper, data, public_key)
}

#' @rdname sm2_encrypt
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_wrapper
sm2_decrypt <- function(data, private_key) {
  .Call(decrypt_wrapper, data, private_key)
}

#' @title SM2 Encrypt/Decrypt - c1c2c3
#' @description
#' The result of SM2 asymmetric encryption consists of three parts: C1, C2, and C3.
#' Among them, C1 is the elliptic curve point calculated based on a generated
#' random number, C2 is the ciphertext data, and C3 is the digest value of SM3.
#' Regarding the two modes of C1C2C3 and C1C3C2, the original Chinese national
#' standard specified the order of C1C2C3, while the new standard follows the
#' order of C1C3C2. These two different order modes are mainly designed to
#' facilitate the parsing and processing of SM2 encryption results across
#' different systems and environments.
#' @rdname sm2_encrypt_c1c2c3
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param public_key a public key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_encrypt_c1c2c3}}{returns a raw vector of the cipher text in the order of c1c2c3}
#'   \item{\link{sm2_decrypt_c1c2c3}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## encrypt and decrypt as c1c2c3
#' keypair <- sm2_gen_keypair()
#' private_key <- keypair$private_key
#' public_key <- keypair$public_key
#' data <- 'abc' |> charToRaw()
#' enc <- sm2_encrypt_c1c2c3(data, public_key)
#' enc
#' dec <- sm2_decrypt_c1c2c3(enc, private_key)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_c1c2c3_wrapper
sm2_encrypt_c1c2c3 <- function(data, public_key) {
  .Call(encrypt_wrapper, data, public_key)
}

#' @rdname sm2_encrypt_c1c2c3
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_c1c2c3_wrapper
sm2_decrypt_c1c2c3 <- function(data, private_key) {
  .Call(decrypt_wrapper, data, private_key)
}

#' @title SM2 Encrypt/Decrypt - asn.1
#' @description
#' According to the usage specifications of the SM2 algorithm, the encrypted
#' data should be encoded using ASN.1, specifically including XCoordinate,
#' YCoordinate, HASH, and CipherText. Among them, XCoordinate and YCoordinate
#' each occupy 32 bytes, HASH occupies 32 bytes, and CipherText is the same
#' length as the plaintext, plus a one-byte "04" identifier. After SM2
#' encryption and ASN.1 encoding, the ciphertext data will be 97 bytes longer
#' than the original plaintext data.
#' @rdname sm2_encrypt_asn1
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param public_key a public key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_encrypt_asna1}}{returns a raw vector of the cipher text in the asn.1 encoding}
#'   \item{\link{sm2_decrypt_asna1}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## encrypt and decrypt as asn.1
#' keypair <- sm2_gen_keypair()
#' private_key <- keypair$private_key
#' public_key <- keypair$public_key
#' data <- 'abc' |> charToRaw()
#' enc <- sm2_encrypt_asna1(data, public_key)
#' enc
#' dec <- sm2_decrypt_asna1(enc, private_key)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_asna1_wrapper
sm2_encrypt_asna1 <- function(data, public_key) {
  .Call(encrypt_asna1_wrapper, data, public_key)
}

#' @rdname sm2_encrypt_asn1
#' @param data data to be encrypted or decrypted, must be a raw vector
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_asna1_wrapper
sm2_decrypt_asna1 <- function(data, private_key) {
  .Call(decrypt_asna1_wrapper, data, private_key)
}

#' @title SM2 Encrypt/Decrypt - hex and base64
#' @description
#' For ease of use, we have provided functions to encrypt data into hex or
#' base64 format and decrypt them from these formats.
#' @rdname sm2_encrypt_hex_base64
#' @param data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param public_key a public key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_encrypt_hex}}{returns a hex string of the cipher text}
#'   \item{\link{sm2_decrypt_hex}}{returns a raw vector of the plain text}
#'   \item{\link{sm2_encrypt_base64}}{returns a base64 string of the cipher text}
#'   \item{\link{sm2_decrypt_base64}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## encrypt and decrypt from hex string
#' keypair <- sm2_gen_keypair()
#' private_key <- keypair$private_key
#' public_key <- keypair$public_key
#' data <- 'abc' |> charToRaw()
#' enc <- sm2_encrypt_hex(data, public_key)
#' enc
#' dec <- sm2_decrypt_hex(enc, private_key)
#' dec
#' enc <- sm2_encrypt_base64(data, public_key)
#' enc
#' dec <- sm2_decrypt_base64(enc, private_key)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_hex_wrapper
sm2_encrypt_hex <- function(data, public_key) {
  .Call(encrypt_hex_wrapper, data, public_key)
}

#' @rdname sm2_encrypt_hex_base64
#' @param data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_hex_wrapper
sm2_decrypt_hex <- function(data, private_key) {
  .Call(decrypt_hex_wrapper, data, private_key)
}

#' @rdname sm2_encrypt_hex_base64
#' @param data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param public_key a public key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR encrypt_base64_wrapper
sm2_encrypt_base64 <- function(data, public_key) {
  .Call(encrypt_base64_wrapper, data, public_key)
}

#' @rdname sm2_encrypt_hex_base64
#' @param data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_base64_wrapper
sm2_decrypt_base64 <- function(data, private_key) {
  .Call(decrypt_base64_wrapper, data, private_key)
}

#' @title SM2 Encrypt/Decrypt - file
#' @description
#' For ease of use, we have provided functions to encrypt or decrypt data directly from files.
#' @rdname sm2_encrypt_file
#' @param data data to be encrypted, must be a raw vector
#' @param enc_file the enctypted file to be saved
#' @param public_key a public key represented as a hexadecimal string
#' @return 
#' \describe{
#'   \item{\link{sm2_encrypt_to_file}}{returns nothing, an encrypted file will be saved in the specified path}
#'   \item{\link{sm2_decrypt_from_file}}{returns nothing, a decrypted file will be saved in the specified path}
#' }
#' @examples
#' ## encrypt and decrypt from file
#' \dontrun{
#'   data <- 'abc' |> charToRaw()
#'   keypair <- sm2_gen_keypair()
#'   private_key <- keypair$private_key
#'   public_key <- keypair$public_key
#'   sm2_encrypt_to_file(data, 'data.enc', public_key)
#'   sm2_decrypt_from_file('data.enc', private_key)
#' }
#'
#' @export
#' @useDynLib smcryptoR encrypt_to_file_wrapper
sm2_encrypt_to_file <- function(data, enc_file, public_key) {
  .Call(encrypt_to_file_wrapper, data, enc_file, public_key)
  invisible()
}

#' @rdname sm2_encrypt_file
#' @param dec_file the encrypted file to be loaded
#' @param private_key a private key represented as a hexadecimal string
#' @export
#' @useDynLib smcryptoR decrypt_from_file_wrapper
sm2_decrypt_from_file <- function(dec_file, private_key) {
  .Call(decrypt_from_file_wrapper, dec_file, private_key)
}
