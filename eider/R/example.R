#' Obtain filepaths to example data and JSON features
#'
#' Return an absolute path to the example data and JSON features provided with
#' the package. These files are contained in the package `inst/extdata`
#' directory.
#'
#' @param file The filename to return the full path for. Defaults to `NULL`, in
#' which case it will return a vector of all valid filenames.
#'
#' @return A string containing the full path to the file, or a vector of
#' filenames
#' @export
#'
#' @examples
#' eider_example()
#' eider_example("random_ae_data.csv")
eider_example <- function(file = NULL) {
  current_package_name <- "eider"
  if (is.null(file)) {
    dir(system.file("extdata", package = current_package_name))
  } else {
    system.file("extdata",
      file,
      package = current_package_name,
      mustWork = TRUE
    )
  }
}
