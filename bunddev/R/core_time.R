#' Convert timestamps to milliseconds
#'
#' @param value Timestamp as numeric (ms), Date, or POSIXct.
#' @param tz Timezone for Date/POSIXct conversion.
#'
#' @details
#' This helper standardizes timestamps for APIs that expect epoch milliseconds.
#' Dates are interpreted in the supplied timezone.
#'
#' @examples
#' bunddev_timestamp_to_ms(as.POSIXct("2024-01-01 00:00:00", tz = "Europe/Berlin"))
#'
#' @return Numeric timestamp in milliseconds.
#' @export
bunddev_timestamp_to_ms <- function(value, tz = "Europe/Berlin") {
  if (length(value) != 1) {
    cli::cli_abort("Timestamp must be length 1.")
  }
  if (inherits(value, "POSIXct")) {
    return(as.numeric(value) * 1000)
  }
  if (inherits(value, "Date")) {
    return(as.numeric(as.POSIXct(value, tz = tz)) * 1000)
  }
  if (is.numeric(value)) {
    return(as.numeric(value))
  }

  cli::cli_abort("Unsupported timestamp type. Use numeric, Date, or POSIXct.")
}

#' Convert milliseconds to POSIXct
#'
#' @param value Timestamp in milliseconds.
#' @param tz Timezone for conversion.
#'
#' @details
#' Converts epoch milliseconds to POSIXct in the requested timezone.
#'
#' @examples
#' bunddev_ms_to_posix(1704067200000, tz = "Europe/Berlin")
#'
#' @return POSIXct timestamp.
#' @export
bunddev_ms_to_posix <- function(value, tz = "Europe/Berlin") {
  if (is.null(value)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  as.POSIXct(numeric_value / 1000, origin = "1970-01-01", tz = tz)
}
