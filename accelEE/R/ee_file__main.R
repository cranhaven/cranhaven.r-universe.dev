#' Run a pre-specified processing scheme
#'
#' Pre-specified routines are designed to facilitate replication of methods from
#' prior studies
#'
#' @param filename character. Path to the file
#' @param scheme character. Name of the routine to be executed. Currently the
#'   only option is \code{"Hibbing 2023"}.
#' @param ... Arguments passed to sub-routine functions
#'
#' @seealso \code{\link{hibbing23-file}}
#'
#' @return A data frame whose contents are prepared according to the
#'   indicated scheme
#' @export
#'
#' @examples
#'   f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'   ee_file(f)
ee_file <- function(filename, scheme = "Hibbing 2023", ...) {

  scheme <- match.arg(scheme)

  switch(
    scheme,
    "Hibbing 2023" = ee_file_hibbing23(filename, ...),
    stop(
      "No preparation routine exists for",
      " `scheme = ", scheme, "`", call. = FALSE
    )
  )

}
