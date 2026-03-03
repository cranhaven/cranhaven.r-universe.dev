#' Run archived R scripts.
#'
#' @param package A scalar character of the package name.
#' @inheritParams targets::tar_source
#'
#' @inherit targets::tar_source return
#'
#' @export
tar_source_archive <- function(
  package,
  files = "R",
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
) {
  files <- system.file(
    "tarchives",
    files,
    package = package,
    mustWork = TRUE
  )
  targets::tar_source(
    files = files,
    envir = envir,
    change_directory = change_directory
  )
}
