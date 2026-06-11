#' @title Normalize GGIR dates to ISO format 
#' @description Convert dates from heterogeneous GGIR output formats into a standard
#' ISO 8601 date format (\code{"YYYY-MM-DD"}). This function performs
#' format harmonization only and does not apply any time zone conversion.
#'
#' @param x A vector of dates. Can be of class \code{character}, \code{factor},
#'   \code{numeric}, \code{Date}, or \code{POSIXct}. Mixed formats are allowed.
#'
#' @details
#' GGIR summary files often contain dates stored as formatted strings,
#' Excel serial numbers, R Date numbers, or POSIX timestamps.
#' This function detects the input type and converts all values into
#' a consistent ISO date representation (\code{"YYYY-MM-DD"}), suitable
#' for merging and aligning GGIR outputs.
#'
#' Numeric values between 20000 and 60000 are treated as Excel date serials
#' (origin \code{"1899-12-30"}). Other numeric values are treated as R Date
#' values (origin \code{"1970-01-01"}).
#'
#' @return A character vector of dates formatted as \code{"YYYY-MM-DD"}.
#'
#' @examples
#' NormalizeGGIRDate("2019/01/03")
#' NormalizeGGIRDate("03-01-2019")
#' NormalizeGGIRDate(43466)        # Excel date
#' NormalizeGGIRDate(Sys.time())   # POSIXct
#' 
#'
#' @export
#'


NormalizeGGIRDate <- function(x) {
  
  if (is.factor(x)) x <- as.character(x)
  
  # POSIXct / POSIXlt
  if (inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%Y-%m-%d"))
  }
  
  # Date
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  
  # Numeric
  if (is.numeric(x)) {
    if (all(x > 20000 & x < 60000, na.rm = TRUE)) {
      d <- as.Date(x, origin = "1899-12-30")
    } else {
      d <- as.Date(x, origin = "1970-01-01")
    }
    return(format(d, "%Y-%m-%d"))
  }
  
  # Character
  x <- trimws(x)
  
  # Remove ISO time and timezone if present
  # e.g. 2017-10-31T05:15:00-0400 ? 2017-10-31
  x <- sub("T.*$", "", x)
  
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
    "%d-%m-%Y", "%d/%m/%Y",
    "%m-%d-%Y", "%m/%d/%Y",
    "%Y%m%d",
    "%d-%b-%Y", "%d-%B-%Y"
  )
  
  d <- as.Date(x, tryFormats = formats)
  format(d, "%Y-%m-%d")
}
