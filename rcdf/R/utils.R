#' Create an empty \code{rcdf} object
#'
#' Initializes and returns an empty \code{rcdf} object. This is a convenient constructor
#' for creating a new \code{rcdf}-class list structure.
#'
#' @param ... Optional elements to include in the list. These will be passed to
#'   the internal list constructor and included in the resulting \code{rcdf} object.
#'
#' @return A list object of class \code{rcdf}.
#' @export
#'
#' @examples
#' rcdf <- rcdf_list()
#' class(rcdf)

rcdf_list <- function(...) {
  value <- list(...)
  as_rcdf(value)
}


#' Convert to \code{rcdf} class
#'
#' Converts an existing list or compatible object into an object of class \code{rcdf}.
#'
#' @param data A list or object to be converted to class \code{rcdf}.
#'
#' @return The input object with class set to \code{rcdf}.
#' @export
#'
#' @examples
#' my_list <- list(a = 1, b = 2)
#' rcdf_obj <- as_rcdf(my_list)
#' class(rcdf_obj)

as_rcdf <- function(data) {
  set_class(data, class_name = 'rcdf')
}


#' Generate RSA key pair and save to files
#'
#' This function generates an RSA key pair (public and private) and saves them to specified files.
#'
#' @param path A character string specifying the directory path where the key files in \code{.pem} format should be saved.
#' @param ... Additional arguments passed to the \code{openssl::rsa_keygen()} function, such as key size.
#' @param password A character string specifying the password for the private key. If \code{NULL}, the private key will not be encrypted.
#' @param which A character string specifying which key to return. Can be either \code{"public"} or \code{"private"}. Default is \code{"public"}.
#' @param prefix A character string used as a prefix for the key file names. Defaults to \code{NULL}, which will result in no prefix.
#'
#' @return A character string representing the file path of the generated key (either public or private, based on the \code{which} argument).
#'
#' @export
#'
#' @examples
#' # Generate both public and private RSA keys and save them to the temp directory
#' path_to <- tempdir()
#' generate_rsa_keys(path = path_to, password = "securepassword")
#'

generate_rsa_keys <- function(path, ..., password = NULL, which = "public", prefix = NULL) {
  key <- openssl::rsa_keygen(...)
  path_to <- list(
    public = file.path(path, paste0(c(prefix, "public-key.pem"), collapse = "-")),
    private = file.path(path, paste0(c(prefix, "private-key.pem"), collapse = "-"))
  )
  openssl::write_pem(key$pubkey, path = path_to$public)
  openssl::write_pem(key, path = path_to$private, password = password)

  return(path_to[[which]])
}



#' Generate a random password
#'
#' This function generates a random password of a specified length. It includes
#' alphanumeric characters by default and can optionally include special characters.
#'
#' @param length Integer. The length of the password to generate. Default is \code{16}.
#' @param special_chr Logical. Whether to include special characters
#'   (e.g., `!`, `@`, `#`, etc.) in the password. Default is \code{TRUE}.
#'
#' @return A character string representing the generated password.
#' @export
#'
#' @examples
#' generate_pw()
#' generate_pw(32)
#' generate_pw(12, special_chr = FALSE)

generate_pw <- function(length = 16, special_chr = TRUE) {

  pw <-c(0:9, rep(letters, 2), rep(LETTERS, 2))

  if(special_chr) {
    pw <- c(pw, "!", "#", "@", "$", "%", "&", "^", "-", "_", "=", "(", ")", "*", "+", "?", "<", ">")
  }

  paste0(sample(pw, length), collapse = "")

}



set_class <- function(data, class_name) {
  class(data) <- c(class(data), class_name)
  return(data)
}


is_rcdf <- function(data) {
  inherits(data, 'rcdf') & inherits(data, 'list')
}



check_if_rcdf <- function(data) {
  if(!is_rcdf(data)) {
    stop('Not a valid RCDF data file')
  }
}


raw_to_hex <- function(x) {
  if (!is.raw(x)) {
    stop("Input must be a raw vector.")
  }

  hex_string <- paste0(sprintf("%02X", as.integer(x)), collapse = "")

  return(hex_string)
}


hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
}


generate_aes_key <- function(passphrase = "") {

  key <- raw_to_hex(openssl::aes_keygen())
  if(passphrase == '') { passphrase <- as.character(Sys.Date()) }

  salt <- openssl::sha256(passphrase)

  value <- list(
    aes_key = as.character(openssl::sha256(paste0(key, salt))),
    aes_iv = as.character(raw_to_hex(openssl::rand_bytes(16)))
  )

  return(value)

}



dir_create_new <- function(path, parent_dir = NULL) {
  if(!is.null(parent_dir)) {
    path <- file.path(path, parent_dir)
  }
  fs::dir_create(path)
  return(path)
}



decrypt_info_aes <- function(data, key = list()) {

  data <- openssl::base64_decode(data)

  aes_key <- key$aes_key
  aes_iv <- key$aes_iv

  if(is.null(aes_key)) {
    return(unserialize(data))
  }

  if(is.null(aes_iv)) {

    value <- openssl::aes_cbc_decrypt(
      data = data,
      key = openssl::sha256(charToRaw(aes_key)),
      iv = NULL
    )

    return(unserialize(value))

  }

  value <- openssl::aes_cbc_decrypt(
    data = data,
    key = openssl::sha256(charToRaw(aes_key)),
    iv = hex_to_raw(aes_iv)
  )

  unserialize(value)

}



extract_key <- function(meta) {

  key <- meta$key_app
  key_admin <- meta$key_admin

  iv <-  meta$iv_app
  iv_admin <-  meta$iv_admin

  if(!is.null(key_admin)) {
    key <- key_admin
  }

  if(!is.null(iv_admin)) {
    iv <- stringr::str_split_i(iv_admin, pattern = '>>><<<', i = 1)
  }

  return(list(key = key, iv = iv))

}




encrypt_info_rsa <- function(data, pub_key) {
  data |>
    serialize(connection = NULL) |>
    openssl::rsa_encrypt(pubkey = pub_key) |>
    openssl::base64_encode()
}



decrypt_info_rsa <- function(data, prv_key, password = NULL) {

  if(!is.null(password)) {
    key <- openssl::read_key(file = prv_key, password = password)
  } else {
    key <- openssl::read_key(file = prv_key)
  }

  data |>
    openssl::base64_decode() |>
    openssl::rsa_decrypt(key) |>
    unserialize()
}



get_pc_metadata <- function(which) {

  values <- list()

  pc <- Sys.info()

  values$pc_os <- tolower(pc[["sysname"]])
  values$pc_user <- pc[["user"]]
  values$pc_effective_user <- pc[["effective_user"]]
  values$pc_os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")
  values$pc_os_version <- pc[["release"]]
  values$pc_hardware <- pc[["machine"]]
  values$pc_pid <- Sys.getpid()

  values[[which]]

}



