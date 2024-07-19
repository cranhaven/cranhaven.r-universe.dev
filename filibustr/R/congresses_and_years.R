#' Calculate the current Congress number
#'
#' This function gives the number of the Congress for the
#' current calendar year, using [Sys.Date()].
#'
#' A new Congress begins in every odd-numbered year, starting in 1789.
#' For example, 2021-2022 was the 117th Congress.
#'
#' @returns A positive whole number.
#'
#' @export
#'
#' @examples
#' current_congress()
#'
current_congress <- function() {
  congress_in_year(Sys.Date())
}

#' Calculate the Congress number of a given year
#'
#' This function gives the number of the Congress for a specified calendar year.
#'
#' @inherit current_congress details
#' @param year Either a number or a Date object.
#'  Cannot be earlier than 1789, the year of the first Congress.
#'
#' @returns A positive whole number.
#' @export
#'
#' @examples
#' congress_in_year(1800)
#' congress_in_year(2022)
congress_in_year <- function(year) {
  if (!(is.numeric(year) | inherits(year, "Date"))) {
    stop("Must provide the year as a number or Date object.")
  }
  # handle Date objects
  if (inherits(year, "Date")) {
    year <- as.numeric(format(year, "%Y"))
  }
  if (year < 1789) {
    stop("The provided year (", year, ") is too early. The first Congress started in 1789.")
  }
  floor((year - 1787) / 2)
}

#' Get the starting year of a Congress
#'
#' This function gives the first year for a specified Congress number.
#'
#' @inherit current_congress details
#'
#' @param congress A positive whole number.
#'
#' @returns A positive whole number, representing the first year of the given Congress.
#'  This year will always be an odd number.
#' @export
#'
#' @examples
#' year_of_congress(1)
#' year_of_congress(118)
year_of_congress <- function(congress) {
  if (!(is.numeric(congress) && congress == as.integer(congress))) {
    stop("Must provide the Congress number as a positive whole number.")
  }
  if (congress < 1) {
    stop("Invalid Congress number (", congress, "). ",
         "The Congress number must be a positive whole number.")
  }
  if (congress >= 1789) {
    warning("That Congress number looks more like a year. ",
            "Did you mean `congress_in_year(", congress, ")`?")
  }
  1787 + 2 * congress
}
