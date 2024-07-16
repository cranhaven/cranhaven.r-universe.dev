#' @importFrom icesConnect ices_get_jwt
#' @importFrom httr content
sd_get <- function(url, retry = TRUE, quiet = FALSE, verbose = FALSE, content = TRUE, use_token = FALSE) {

  resp <-
    ices_get_jwt(
      url,
      retry = retry, quiet = quiet, verbose = verbose,
      jwt = if (use_token) NULL else ""
    )

  if (content) {
    content(resp, simplifyVector = TRUE)
  } else {
    resp
  }
}

api_url <- function() {
  # make an option?
  "https://sd.ices.dk/services"
}
