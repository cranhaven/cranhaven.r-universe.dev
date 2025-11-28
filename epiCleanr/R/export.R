#' Export Data to Various File Formats
#'
#' This function provides a unified interface for exporting data to various
#' file formats supported by the \code{\link[=rio]{rio::export()}}
#' function. The format is automatically detected from the file extension to
#' simplify the exporting process.
#'
#' @param data The dataset to be exported.
#' @param file_path Character string specifying the path to the output file.
#' @param ... Additional arguments to be passed to the underlying write
#'   functions. These arguments are specific to the file format being exported.
#'   Please refer to the documentation of each package used for more
#'   information.
#'
#' @return No return value, called for side effects.
#'
#' @seealso \code{\link[=rio]{rio::export()}}, which this function is
#' based on.
#'
#' @examples
#' # Create temporary account
#' tmpdir <- tempfile()
#' dir.create(tmpdir)
#'
#' # Export a CSV file
#' export(mtcars, file_path = file.path(tmpdir, "file.csv"))
#'
#' # Export an Excel file
#' export(mtcars, file_path = file.path(tmpdir, "file.xlsx"))
#'
#' # Export a Stata DTA file
#' export(mtcars, file_path = file.path(tmpdir, "file.dta"))
#'
#' # Export an RDS file
#' export(mtcars, file_path = file.path(tmpdir, "file.rds"))
#'
#' # Export an RData file
#' export(list(mtcars = mtcars, iris = iris),
#'        file_path = file.path(tmpdir, "file.RData"))
#'
#' # Remove the temporary directory and its contents
#' unlink(tmpdir, recursive = TRUE)

#' @importFrom rio import
#' @importFrom rio install_formats
#' @importFrom tools file_ext
#'
#' @export
export <- function(data, file_path, ...) {

  # Extract the file extension from the input file path
  file_ext <- tools::file_ext(file_path)

  # List of supported formats
  supported_formats <- c(
    "csv", "tsv", "xlsx", "rds", "RData", "dta"
  )

  if (file_ext %in% supported_formats) {
    rio::export(data, file_path, ...)
  } else {
    stop(
      paste(
        "File format '", file_ext, "' not supported by 'rio'.",
        "Please refer to the package documentation for a full list",
        "of supported formats."
      )
    )
  }
}
