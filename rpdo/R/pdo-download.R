#' Download PDO Index Data
#'
#' Downloads PDO index data from the no longer updated
#' <http://research.jisao.washington.edu/pdo/PDO.latest>.
#'
#' \lifecycle{soft-deprecated}
#'
#' Deprecated for [rsoi::download_pdo()].
#'
#' `download_pdo()` is an alias for `pdo_download()`.
#'
#' @return A data frame of the PDO index data.
#' @export
pdo_download <- function() {
  lifecycle::deprecate_warn(
    "0.3.1", "pdo_download()", "rsoi::download_pdo()",
    id = "pdo_download"
  )

  pdo <- get_pdo()
  pdo <- clean_pdo(pdo)
  pdo <- read_pdo(pdo)
  pdo <- tidy_pdo(pdo)
  check_pdo(pdo)
}

#' @rdname pdo_download
#' @export
download_pdo <- function() {
  lifecycle::deprecate_warn(
    "0.3.1", "pdo_download()", "rsoi::download_pdo()",
    id = "pdo_download"
  )

  pdo_download()
}
