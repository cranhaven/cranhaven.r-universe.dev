#' @title Find API Key for Leanpub
#'
#' @description Determines if Leanpub API Key is set.
#' If not, it stops and returns an error.  If so, returns the value.
#' @param api_key Leanpub API key (\code{Sys.getenv('LEANPUB_API_KEY')})
#' @param error Should the function error if \code{api_key = NULL}?
#' @note You can either set the API key or have it accessible by
#' \code{api_key = Sys.getenv('LEANPUB_API_KEY')}.
#' @return Character API key
#' @export
#' @examples
#' res = lp_api_key(error = FALSE)
#' lp_have_api_key()
lp_api_key = function(api_key = NULL, error = TRUE) {
  if (is.null(api_key)) {
    api_key = Sys.getenv("LEANPUB_API_KEY")
    if (api_key %in% "") {
      api_key = NULL
    }
  }

  if (!is.null(api_key)) {
    if (api_key %in% "") {
      api_key = NULL
    }
  }

  if (is.null(api_key) & error) {
    stop(paste0("API key not found, please set ",
                "set environment variable LEANPUB_API_KEY or in .Renviron",
                " to be ",
                "accessed by Sys.getenv('LEANPUB_API_KEY')"))
  }
  return(api_key)
}


#' @rdname lp_api_key
#' @export
lp_have_api_key = function(api_key = NULL) {
  api_key = lp_api_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}
