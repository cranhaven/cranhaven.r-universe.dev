#' LC-MS/MS sample data
#'
#' A `mpactr` R6 class object containing a feature table and associated sample
#' metadata.
#'
#' @return An `mpactr` R6 class object.
#' @format ## `culture_data`
#' A `mpactr` with 2 attributes:
#' \describe{
#'  \item{peak_table}{A feature table of class `data.table`}
#'  \item{meta_data}{A `data.table` with associated sample metadata}
#'  }
"cultures_data"


#' Get file paths for examples
#'
#' mpactr contains a number of example files in the `inst/extdata` directory.
#' This function makes them accessible in documentation that shows how file
#' paths are used in function examples.
#'
#' @param file Name of a file. If `NULL`, all examples files will be listed.
#'
#' @export
#' @return A file path to example data stored in the `inst/extdata` directory
#' of the package.
#' @examples
#' example_path()
#'
#' example_path("metadata.csv")
example_path <- function(file = NULL) {
  path <- ""
  if (is.null(file)) {
    path <- dir(system.file("extdata", package = "mpactr"))
  } else {
    path <- system.file("extdata", file, package = "mpactr", mustWork = TRUE)
  }
  return(path)
}
