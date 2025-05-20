#' return TRUE if spip exists where it should be installed.
#'
#' This just checks for the file where it gets installed with
#' `install_spip()`. This is exported so it can be used to control
#' the flow in the vignettes, so that vignettes can still be
#' written using saved results when spip is not installed (i.e.,
#' at CRAN, etc.)
#' @export
#' @return A single logical scalar, either TRUE or FALSE.
#' @examples
#' # will be FALSE unless spip has been externally installed
#' spip_exists()
spip_exists <- function() {
  file.exists(spip_binary_path())
}

