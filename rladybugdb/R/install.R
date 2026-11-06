#' Install the LadybugDB C library (deprecated)
#'
#' This function is deprecated. The C library is built from bundled source
#' during package installation and no Python installation is required. This
#' helper only issues a deprecation warning; it does not install any software.
#'
#' @param ... Ignored.
#'
#' @return Invisible `NULL`, called for the side effect of issuing a
#'   deprecation warning. No software is installed.
#'
#' @export
ladybugdb_install <- function(...) {
  .Deprecated(
    msg = paste0(
      "`ladybugdb_install()` is deprecated.\n",
      "The C library is now bundled; no Python installation is required."
    )
  )
  invisible(NULL)
}

#' Check whether LadybugDB is available
#'
#' Returns `TRUE` if the native LadybugDB C library is loaded and functional.
#'
#' @return A logical scalar: `TRUE` if the native LadybugDB C library responds
#'   to a version query, and `FALSE` if that query fails.
#'
#' @examples
#' ladybugdb_is_installed()
#'
#' @export
ladybugdb_is_installed <- function() {
  tryCatch({
    lb_database_version()
    TRUE
  }, error = function(e) FALSE)
}

#' Return the LadybugDB C library version string
#'
#' @return A character string identifying the bundled LadybugDB engine version,
#'   e.g. `"0.20.4"`.
#'
#' @examples
#' ladybugdb_version()
#'
#' @export
ladybugdb_version <- function() {
  lb_database_version()
}
