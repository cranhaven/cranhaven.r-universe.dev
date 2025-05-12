#' @title SM4 Encrypt/Decrypt
#' @description
#' The SM4 algorithm is a block symmetric encryption algorithm with a block size
#' and key length of 128 bits. Compared to the SM2 algorithm, it has higher
#' encryption and decryption efficiency and can be used to encrypt larger amounts
#' of data. SM4 supports both the ECB (Electronic Codebook) mode and the
#' CBC (Cipher Block Chaining) mode. The ECB mode is a simple block cipher
#' encryption mode that encrypts each data block independently without depending
#' on other blocks. The CBC mode, on the other hand, is a chained block cipher
#' encryption mode where the encryption of each block depends on the previous
#' ciphertext block. Therefore, it requires an initialization vector (IV) of
#' the same 128-bit length. The CBC mode provides higher security than the ECB mode.
#' @rdname sm4_encrypt
#' @param input_data data bytes to be encrypted, must be a raw vector
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @return 
#' \describe{
#'   \item{\link{sm4_encrypt_ecb}}{returns a raw vector of the cipher text using ecb mode}
#'   \item{\link{sm4_decrypt_ecb}}{returns a raw vector of the plain text}
#'   \item{\link{sm4_encrypt_cbc}}{returns a raw vector of the cipher text using cbc mode}
#'   \item{\link{sm4_decrypt_cbc}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## ecb mode
#' data <- 'abc' |> charToRaw()
#' key <- '1234567812345678' |> charToRaw()
#' iv <- '0000000000000000' |> charToRaw()
#' enc <- sm4_encrypt_ecb(data, key)
#' enc
#' dec <- sm4_decrypt_ecb(enc, key)
#' dec
#' ## cbc mode
#' enc <- sm4_encrypt_cbc(data, key, iv)
#' enc
#' dec <- sm4_decrypt_cbc(enc, key, iv)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_ecb_wrapper
sm4_encrypt_ecb <- function(input_data, key) {
  .Call(encrypt_ecb_wrapper, input_data, key)
}

#' @title SM4 Encrypt/Decrypt - hex and base64
#' @description
#' For ease of use, we have provided functions to encrypt data into hex or
#' base64 format and decrypt them from these formats.
#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @return 
#' \describe{
#'   \item{\link{sm4_encrypt_ecb_base64}}{returns a base64 string of the cipher text using ecb mode}
#'   \item{\link{sm4_encrypt_ecb_hex}}{returns a hex string of the cipher text using ecb mode}
#'   \item{\link{sm4_decrypt_ecb_base64}}{returns a raw vector of the plain text}
#'   \item{\link{sm4_decrypt_ecb_hex}}{returns a raw vector of the plain text}
#'   \item{\link{sm4_encrypt_cbc_base64}}{returns a base64 string of the cipher text using cbc mode}
#'   \item{\link{sm4_encrypt_cbc_hex}}{returns a hex string of the cipher text using cbc mode}
#'   \item{\link{sm4_decrypt_cbc_base64}}{returns a raw vector of the plain text}
#'   \item{\link{sm4_decrypt_cbc_hex}}{returns a raw vector of the plain text}
#' }
#' @examples
#' ## SM4 Encrypt/Decrypt - hex and base64
#' data <- 'abc' |> charToRaw()
#' key <- '1234567812345678' |> charToRaw()
#' iv <- '0000000000000000' |> charToRaw()
#' ## ecb mode
#' enc <- sm4_encrypt_ecb_base64(data, key)
#' enc
#' dec <- sm4_decrypt_ecb_base64(enc, key)
#' dec
#' enc <- sm4_encrypt_ecb_hex(data, key)
#' enc
#' dec <- sm4_decrypt_ecb_hex(enc, key)
#' dec
#' ## cbc mode
#' enc <- sm4_encrypt_cbc_base64(data, key, iv)
#' enc
#' dec <- sm4_decrypt_cbc_base64(enc, key, iv)
#' dec
#' enc <- sm4_encrypt_cbc_hex(data, key, iv)
#' enc
#' dec <- sm4_decrypt_cbc_hex(enc, key, iv)
#' dec
#' @export
#' @useDynLib smcryptoR encrypt_ecb_base64_wrapper
sm4_encrypt_ecb_base64 <- function(input_data, key) {
  .Call(encrypt_ecb_base64_wrapper, input_data, key)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @export
#' @useDynLib smcryptoR encrypt_ecb_hex_wrapper
sm4_encrypt_ecb_hex <- function(input_data, key) {
  .Call(encrypt_ecb_hex_wrapper, input_data, key)
}

#' @title SM4 Encrypt/Decrypt - file
#' @description
#' For ease of use, we have provided functions to encrypt or decrypt data directly from files.
#' @rdname sm4_encrypt_file
#' @param input_file the original file for encrypt, or the encrypted file for decrypt
#' @param output_file the encrypted file for encrypt, or the decrypted file for decrypt
#' @param key the key, must be a raw vector of length 16
#' @return 
#' \describe{
#'   \item{\link{sm4_encrypt_ecb_to_file}}{returns nothing, and an encrypted file will be saved in the specified path using ecb mode}
#'   \item{\link{sm4_decrypt_ecb_from_file}}{returns nothing, and a decrypted file will be saved in the specified path using ecb mode}
#'   \item{\link{sm4_encrypt_cbc_to_file}}{returns nothing, and an encrypted file will be saved in the specified path using cbc mode}
#'   \item{\link{sm4_decrypt_cbc_from_file}}{returns nothing, and a decrypted file will be saved in the specified path using cbc mode}
#' }
#' @examples
#' \dontrun{
#'   key <- '1234567812345678' |> charToRaw()
#'   iv <- '0000000000000000' |> charToRaw()
#'   ## ecb mode
#'   sm4_encrypt_ecb_to_file('a.txt', 'a.enc', key)
#'   sm4_decrypt_ecb_from_file('a.enc', 'a.dec', key)
#'   ## cbc mode
#'   sm4_encrypt_cbc_to_file('a.txt', 'a.enc', key, iv)
#'   sm4_decrypt_cbc_from_file('a.enc', 'a.dec', key, iv)
#' }
#' @export
#' @useDynLib smcryptoR encrypt_ecb_to_file_wrapper
sm4_encrypt_ecb_to_file <- function(input_file, output_file, key) {
  .Call(encrypt_ecb_to_file_wrapper, input_file, output_file, key)
  invisible()
}

#' @rdname sm4_encrypt
#' @param input_data data bytes to be encrypted, must be a raw vector
#' @param key the key, must be a raw vector of length 16
#' @export
#' @useDynLib smcryptoR decrypt_ecb_wrapper
sm4_decrypt_ecb <- function(input_data, key) {
  .Call(decrypt_ecb_wrapper, input_data, key)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @export
#' @useDynLib smcryptoR decrypt_ecb_base64_wrapper
sm4_decrypt_ecb_base64 <- function(input_data, key) {
  .Call(decrypt_ecb_base64_wrapper, input_data, key)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @export
#' @useDynLib smcryptoR decrypt_ecb_hex_wrapper
sm4_decrypt_ecb_hex <- function(input_data, key) {
  .Call(decrypt_ecb_hex_wrapper, input_data, key)
}

#' @rdname sm4_encrypt_file
#' @param input_file the original file for encrypt, or the encrypted file for decrypt
#' @param output_file the encrypted file for encrypt, or the decrypted file for decrypt
#' @param key the key, must be a raw vector of length 16
#' @export
#' @useDynLib smcryptoR decrypt_ecb_from_file_wrapper
sm4_decrypt_ecb_from_file <- function(input_file, output_file, key) {
  .Call(decrypt_ecb_from_file_wrapper, input_file, output_file, key)
  invisible()
}

#' @rdname sm4_encrypt
#' @param input_data data bytes to be encrypted, must be a raw vector
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR encrypt_cbc_wrapper
sm4_encrypt_cbc <- function(input_data, key, iv) {
  .Call(encrypt_cbc_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR encrypt_cbc_base64_wrapper
sm4_encrypt_cbc_base64 <- function(input_data, key, iv) {
  .Call(encrypt_cbc_base64_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR encrypt_cbc_hex_wrapper
sm4_encrypt_cbc_hex <- function(input_data, key, iv) {
  .Call(encrypt_cbc_hex_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_file
#' @param input_file the original file for encrypt, or the encrypted file for decrypt
#' @param output_file the encrypted file for encrypt, or the decrypted file for decrypt
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR encrypt_cbc_to_file_wrapper
sm4_encrypt_cbc_to_file <- function(input_file, output_file, key, iv) {
  .Call(encrypt_cbc_to_file_wrapper, input_file, output_file, key, iv)
  invisible()
}

#' @rdname sm4_encrypt
#' @param input_data data bytes to be encrypted, must be a raw vector
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR decrypt_cbc_wrapper
sm4_decrypt_cbc <- function(input_data, key, iv) {
  .Call(decrypt_cbc_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR decrypt_cbc_base64_wrapper
sm4_decrypt_cbc_base64 <- function(input_data, key, iv) {
  .Call(decrypt_cbc_base64_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_hex_base64
#' @param input_data for encrypt, data is a raw vector, for decrypt, data is a hex or base64 string
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR decrypt_cbc_hex_wrapper
sm4_decrypt_cbc_hex <- function(input_data, key, iv) {
  .Call(decrypt_cbc_hex_wrapper, input_data, key, iv)
}

#' @rdname sm4_encrypt_file
#' @param input_file the original file for encrypt, or the encrypted file for decrypt
#' @param output_file the encrypted file for encrypt, or the decrypted file for decrypt
#' @param key the key, must be a raw vector of length 16
#' @param iv the initialization vector, must be a raw vector of 16
#' @export
#' @useDynLib smcryptoR decrypt_cbc_from_file_wrapper
sm4_decrypt_cbc_from_file <- function(input_file, output_file, key, iv) {
  .Call(decrypt_cbc_from_file_wrapper, input_file, output_file, key, iv)
  invisible()
}
