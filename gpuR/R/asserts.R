#' @title Does device have 'double' support?
#' @description Function to query if device (identified by index) supports
#' double precision
#' @param device_idx An integer indicating which device to query
#' @param context_idx An integer indicating which context to query
#' @param severity How severe should the consequences of the assertion be?
#' @return Returns nothing but throws an error if device does not support
#' double precision
#' @seealso \link{deviceHasDouble}
#' @export
assert_has_double <- function(device_idx, context_idx,
                              severity = getOption("assertive.severity", "stop")) {
  msg <- sprintf(
    "The device %s on context %s does not support double. Try setting type = 'float' or change device if multiple available.",
    device_idx, context_idx
  )
  if (!deviceHasDouble(device_idx, context_idx)) {
    stop(msg)
  }
}

# below we create functions corresponding to names in the assertive package.

assert_all_are_in_range <- function(x, lower, upper) {
  if (any(x < lower | x > upper | x == upper, na.rm = TRUE)) {
    stop("out of range")
  }
}

assert_all_are_in_closed_range <- function(x, lower, upper) {
  if (any(x < lower | x > upper, na.rm = TRUE)) {
    stop("out of range")
  }
}

assert_all_are_positive <- function(x) {
  if (any(x <= 0, na.rm = TRUE)) {
    stop("out of range")
  }
}

assert_all_are_true <- function(x) {
  if (!all(x, na.rm = TRUE)) {
    stop("not all are true")
  }
}

assert_are_identical <- function(x, y) {
  if (!identical(x, y)) {
    stop("objects are not identical")
  }
}

assert_engine <- function(condition, ...) {
  if (!condition(...)) {
    stop("assertion failed")
  }
}