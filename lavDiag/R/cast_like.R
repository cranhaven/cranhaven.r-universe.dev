
# Cast a single scalar value to have the "same type" as a prototype vector x
#' @noRd
.cast_like <- function(value, x) {
  if (is.factor(x)) {
    return(factor(value, levels = levels(x), ordered = is.ordered(x)))
  }
  if (inherits(x, "Date"))    return(as.Date(value, origin = "1970-01-01"))
  if (inherits(x, "POSIXct")) return(as.POSIXct(value, tz = attr(x, "tzone") %||% "UTC", origin = "1970-01-01"))
  if (is.integer(x))          return(as.integer(value))
  if (is.numeric(x))          return(as.numeric(value))
  if (is.logical(x))          return(as.logical(value))
  if (is.character(x))        return(as.character(value))
  value
}
