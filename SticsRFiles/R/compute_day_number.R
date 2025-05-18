#' Day of year
#'
#' Returns the day number of the year corresponding to a given date.
#' @param date date you wish to convert, in the \code{\link[base]{Date}} format
#' @return integer
#' @author Timothee Flutre
#' @keywords internal
#' @noRd
get_day_of_year <- function(date){
  stopifnot(methods::is(date, "Date"))

  return(lubridate::yday(date))
}

#' Leap year
#'
#' Returns TRUE (or 1) if the given year is a leap year,
#' FALSE (or 0) otherwise.
#' @param year numeric
#' @param integer logical
#' @return logical or integer, either integer == FALSE (default) or TRUE
#' @author Timothee Flutre
#' @keywords internal
#' @noRd
is_leap_year <- function(year, integer = FALSE){

  is_leap <- year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)

  if (integer) return(as.integer(is_leap))

  return(is_leap)
}

#' Convert date into day number
#'
#' Computes the day number corresponding to a given date (or vector of)
#' from the first day of a start year. Typically, the start year should
#' be the year of a STICS simulation start.
#' Leap years are properly handled.
#' @param date date(s) vector to be converted,
#' in the character format ("YYYY-MM-DD") or \code{\link[base]{Date}} format
#' @param start_year year to be used as time reference (simulation start year).
#' Optional.
#' @param start_date `r lifecycle::badge("deprecated")` `start_date` is no
#'   longer supported, use `start_year` instead.
#' @return numeric vector
#' @author Timothee Flutre
#' @examples
#'
#' date <- as.Date("2015-02-10")
#' compute_day_from_date(date = date)
#'
#' compute_day_from_date(date = "2015-02-10", start_year = 2014)
#'
#' date <- as.Date("2009-02-10")
#' compute_day_from_date(date = date, start_year = 2008 )
#'
#' dates <- c(as.Date("2008-02-10"), as.Date("2009-02-10"))
#' compute_day_from_date(date = dates, start_year = 2008 )
#'
#' @export
compute_day_from_date <- function(date,
                                  start_year = NULL,
                                  start_date = lifecycle::deprecated()){


  # In case of several input dates
  if(length(date) > 1) {
    out <- unlist(
      lapply(date, function(x) {
        compute_day_from_date(date = x,
                              start_year = start_year,
                              start_date = start_date)
      }
      )
    )
    return(out)
  }

  # To keep compatibility with the previous argument start_date
  # conversion to year
  if (lifecycle::is_present(start_date)) {
    lifecycle::deprecate_warn("1.4.0",
                              "compute_day_from_date(start_date)",
                              "compute_day_from_date(start_year)")

    start_year <- lubridate::year(as.Date(start_date))
  }

  stopifnot(methods::is(as.Date(date), "Date") || methods::is(date, "Date"),
            is.null(start_year) || methods::is(start_year, "numeric"))

  # Converting a character date to Date format
  if(is.character(date)) date <- as.Date(date)

  day_of_year <- get_day_of_year(date)
  date_year <- lubridate::year(date)

  # No start_year, or start_year ==  date year
  # returning the current date year day
  if (is.null(start_year))
    start_year <- date_year

  if (date_year == start_year) return(day_of_year)

  # Impossible case
  if(start_year > date_year)
    stop("The start year ", start_year,
         " is greater than the date year ", date_year, "!")

  # Several years
  years <- seq(start_year, date_year - 1)
  leap_year_number <- sum(is_leap_year(years, integer = TRUE))

  # Calculating the day number over years
  return(365 * (length(years) - leap_year_number) +
    366 * leap_year_number +
    day_of_year
    )
}
