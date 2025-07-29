#' Get integer within a range
#'
#' Assures that what is returned is an integer within the specified range.
#' Real values are truncated. Non-numerics are forced to minimum without
#' warning.
#'
#' @return A vector of integers forced to be within the specified range.
#'
#' @param int value to be forced within a range
#' @param minimum minimum integer value.
#' @param maximum maximum integer value
#' @param na if "min" then non-numerics are forced to the minimum in the range
#' If "max" then non-numerics are forced to the maximum in the range.
#' If not either "min" or "max" it is forced to "min".
#' @export
#' @examples
#' library(nprcgenekeepr)
#' withinIntegerRange()
#' withinIntegerRange(, 0, 10)
#' withinIntegerRange(NA, 0, 10, na = "max")
#' withinIntegerRange(, 0, 10, na = "max") # no argument is not NA
#' withinIntegerRange(LETTERS, 0, 10)
#' withinIntegerRange(2.6, 1, 5)
#' withinIntegerRange(2.6, 0, 2)
#' withinIntegerRange(c(0, 2.6, -1), 0, 2)
#' withinIntegerRange(c(0, 2.6, -1, NA), 0, 2)
#' withinIntegerRange(c(0, 2.6, -1, NA), 0, 2, na = "max")
#' withinIntegerRange(c(0, 2.6, -1, NA), 0, 2, na = "min")
withinIntegerRange <- function(int = 0L, minimum = 0L, maximum = 0L,
                               na = "min") {
  if (!na %in% c("min", "max")) {
    na <- "min"
  }
  if (na == "max") {
    naValue <- maximum
  } else {
    naValue <- minimum
  }

  if (is.null(int)) {
    int <- 0L
  }

  int <- suppressWarnings(as.integer(int))
  int <- ifelse(is.na(int), naValue, int)
  int <- pmax(int, minimum)
  pmin(int, maximum)
}
