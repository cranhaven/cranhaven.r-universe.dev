#' Encrypts/Decrypts a vector
#'
#' @description Used internally to encrypt/decrypt the value column of your
#' cookie jar.
#'
#'
#' @param vec A vector of values to encrypt
#' @export
#'
#' @details If you save valuable cookies, for example login information, you
#'   should encrypt them with a personalised password. This can be set with,
#'   e.g., \code{Sys.setenv("COOKIE_KEY" = "megageheim")} or in an
#'   .Renviron file.
#'
#' @returns list of encrypted elements (for \code{encrypt_vec}); vector of
#'   decrypted elements (for \code{encrypt_vec}).
#'
#' @examples
#' enc <- encrypt_vec(c("foo", "bar"))
#' decrypt_vec(enc)
#'
encrypt_vec <- function(vec) {
  key <- openssl::sha256(charToRaw(Sys.getenv("COOKIE_KEY", unset = "supergeheim")))
  lapply(vec, enc, key)
}

#' encrypt a single element
#' @noRd
enc <- function(x, key = NULL) {
  openssl::aes_ctr_encrypt(charToRaw(x), key)
}

#' @rdname encrypt_vec
#' @export
decrypt_vec <- function(vec) {
  key <- openssl::sha256(charToRaw(Sys.getenv("COOKIE_KEY", unset = "supergeheim")))
  vapply(vec, dec, key, FUN.VALUE = character(1))
}

#' decrypt a single element
#' @noRd
dec <- function(x, key = NULL) {
  rawToChar(openssl::aes_ctr_decrypt(x, key))
}
