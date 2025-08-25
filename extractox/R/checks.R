#' Check for NA values in a specific column of a dataframe
#'
#' Checks for NA values in a specified column of a dataframe and optionally warns if any are found.
#'
#' @param dat A dataframe that contains the data.
#' @param col_to_check The name of the column to check for NA values.
#' @param verbose Logical indicating whether to show a warning if NAs are found. Default is TRUE.
#' @keywords internal
#' @noRd
check_na_warn <- function(dat, col_to_check, verbose = TRUE) {
  ids_not_found <- dat$query[is.na(dat[[col_to_check]])]

  if (all(isTRUE(verbose), length(ids_not_found) != 0)) {
    cli::cli_warn("Chemical{?s} {.field {ids_not_found}} not found!")
  }

  invisible(ids_not_found)
}


#' Check the status code of an HTTP response
#'
#' This function checks the status code of an HTTP response and provides
#' appropriate messages based on the status.
#'
#' @param resp An HTTP response object from the httr2 package.
#' @param verbose A logical value indicating whether to print detailed
#'    messages. Default is TRUE.
#' @keywords internal
#' @noRd
#' @return This function does not return a value. It is used for its
#'    side effects.
check_status_code <- function(resp, verbose = TRUE) {
  status_code <- httr2::resp_status(resp)
  if (!status_code %in% c(200L, 202L)) {
    cli::cli_abort("Request failed with status code: {status_code}")
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Request succeeded with status code: {status_code}")
    }
  }
}


#' Check Internet
#'
#' Wrapper around `pingr::is_online` to print message
#' a better message.
#'
#' @param verbose Boolean to display messages.
#' @keywords internal
#' @noRd
check_internet <- function(verbose = TRUE) {
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Checking Internet Connection...")
  }

  if (isFALSE(pingr::is_online())) {
    cli::cli_abort("It seems that you are not connected to internet!")
    out <- FALSE
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Internet connection OK...")
    }
    out <- TRUE
  }
  invisible(out)
}


#' Check for problematic libcurl with OpenSSL
#'
#' Checks if the system's libcurl version is 7.78.0 or newer and linked
#' against OpenSSL. This combination can cause issues with servers that do not
#' support secure legacy renegotiation, such as those from the EPA.
#' @return A boolean value: `TRUE` if the combination is problematic, `FALSE`
#'   otherwise.
#' @keywords internal
#' @noRd
check_need_libcurl_condathis <- function() {
  libcurl_safe <- TRUE

  lib_curl_version <- curl::curl_version()

  # Check if the version is greater than or equal to 7.78.0
  # and if the SSL version is OpenSSL
  # If both conditions are met, set libcurl_safe to FALSE


  attr_lib_curl_version <- attributes(lib_curl_version)$ssl_version

  if (all(
    lib_curl_version$version >= "7.78.0",
    grepl("OpenSSL", lib_curl_version$ssl_version)
  )) {
    libcurl_safe <- FALSE
  }

  isFALSE(libcurl_safe)
}

#' Install a compatible curl version using condathis
#'
#' Uses the `{condathis}` package to install a specific version of curl
#' (`7.78.0`) into a dedicated conda environment named "openssl-linux-env".
#' This is used to work around issues with modern libcurl versions and EPA servers.
#'
#' @param verbose Logical indicating whether to show installation messages.
#'   Default is `TRUE`.
#' @return This function is called for its side effects and does not return a
#'   value.
#' @keywords internal
#' @noRd
condathis_downgrade_libcurl <- function(verbose = "silent") {
  if (!condathis::env_exists("openssl-linux-env")) {
    condathis::create_env(
      c("curl==7.78.0", "libcurl", "openssl"),
      env_name = "openssl-linux-env",
      verbose = verbose
    )
  }
}
