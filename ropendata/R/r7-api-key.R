#' Get or set RAPID7_OPENDATA_API_KEY value
#'
#' The API wrapper functions in this package all rely on a Rapid7 Open Data PI key residing in
#' the environment variable \code{RAPID7_OPENDATA_API_KEY}. The easiest way to accomplish this
#' is to set it in the `.Renviron` file in your home directory.
#'
#' API requests are authenticated by a key which can be found and managed from your user
#' profile. With every call to the API, the user must be authenticated. The API has a
#' simple means to do this by passing in your key as an HTTP “Authorization” header with
#' the request.
#'
#' When you signed up for Rapid7 Open Data, a ‘default-key’ was generated for your convenience.
#' This is the key that is used throughout the example code within algorithm pages. For
#' these examples to work correctly, this default key must exist with all the permissions,
#' otherwise the usage examples may result in a 401 Unauthorized error.
#'
#' You need a Rapid7 account to get an API key. You can request a free account via
#' <https://opendata.rapid7.com/#register> and then navigate to the "Open Data API"
#' link there to create both an organizational key and a user key. You can only
#' use **user keys** with the Open Data API and you will receive error messages
#' indicating so if you try to use an organizational key.
#'
#' @note As the API documentation says, you need a **User** key vs an **Org** key.
#' @param force Force setting a new Rapid7 Open Data API key for the current environment?
#' @return atomic character vector containing the Rapid7 Open Data API key
#' @references \url{https://opendata.rapid7.com/apihelp/}
#' @export
rapid7_api_key <- function(force = FALSE) {

  env <- Sys.getenv('RAPID7_OPENDATA_API_KEY')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var RAPID7_OPENDATA_API_KEY to your Rapid7 Open Data API key",
      call. = FALSE)
  }

  message("Couldn't find env var RAPID7_OPENDATA_API_KEY See ?rapid7_api_key for more details.")
  message("Please enter your Rapid7 Open Data API key and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("Rapid7 Open Data API key entry failed", call. = FALSE)
  }

  message("Updating RAPID7_OPENDATA_API_KEY env var...")
  Sys.setenv(RAPID7_OPENDATA_API_KEY = pat)

  pat

}
