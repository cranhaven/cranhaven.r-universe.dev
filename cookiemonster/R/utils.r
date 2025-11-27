#' @noRd
url_get_domain <- function(x) {
  x <- sub("^#HttpOnly_\\.|^\\.", "", x)
  urltools::domain(x)
}
