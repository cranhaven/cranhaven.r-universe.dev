#' Use tarchives
#'
#' Set up tarchives for an existing package.
#'
#' @inheritParams targets::tar_make
#'
#' @return No return value, called for side effects.
#'
#' @export
use_tarchives <- function(store = targets::tar_config_get("store")) {
  fs::dir_create("inst/tarchives")
  usethis::use_build_ignore(
    paste0("^inst/tarchives/.*/", store, "$"),
    escape = FALSE
  )
}
