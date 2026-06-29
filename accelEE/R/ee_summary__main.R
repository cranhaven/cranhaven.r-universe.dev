#' Run a pre-specified summary scheme
#'
#' Pre-specified routines are designed to facilitate replication of methods from
#' prior studies
#'
#' @param d input data (presumably a data frame)
#' @param scheme character. Name of the routine to be executed. Currently the
#'   only option is \code{"Hibbing 2023"}.
#' @param ... Arguments passed to sub-routine functions
#'
#' @seealso Subroutine function(s): \code{\link{hibbing23-summary}}
#'
#' @return A data frame whose contents are prepared according to the
#'   indicated scheme
#' @export
#'
#' @examples
#'
#' f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'
#' ## Read acceleration data
#'   accel <- ee_file(f)
#'
#' ## Generate some bogus count data
#'   cts <- data.frame(
#'     Timestamp = accel$Timestamp,
#'     Axis1 = 0, Axis2 = 0, Axis3 = 0,
#'     valid_status = "Non-Wear"
#'   )
#'
#' ## Returns NA with a warning about no complete days in the file
#'   suppressWarnings(ee_summary(
#'     merge(accel, cts)
#'   ))
#'
ee_summary <- function(d, scheme = "Hibbing 2023", ...) {

  scheme <- match.arg(scheme)

  switch(
    scheme,
    "Hibbing 2023" = ee_summary_hibbing23(d, ...),
    stop(
      "No summary routine exists for",
      " `scheme = ", scheme, "`", call. = FALSE
    )
  )

}
