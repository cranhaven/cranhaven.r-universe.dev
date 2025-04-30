#' @keywords internal
#' @noRd

as_difftime <- function (timeseries, format = "%X", units = "auto") {
  if (units %in% c("months", "years")) {
    if (!is.numeric(timeseries)) {
      stop("'timeseries' must be numeric for units = '", units, "'", sep = "")
    } else {
      structure(timeseries, units = units, class = "difftime")
    }
  } else base::as.difftime(timeseries, format, units)
}
