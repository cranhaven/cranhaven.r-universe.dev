RELEASE_URL <- "https://github.com/john-b-edwards/cbbd-data/releases/download"

#' get the most recent CBB season
#'
#' @description For a given date (defaults to the system date), returns the most recent or "current" college basketball season.
#'
#' @param date the date to obtain the most recent season of. Defaults to the system date.
#'
#' @return the most recent CBB season as an integer
#'
#' @examples
#' \donttest{
#' most_recent_season()
#'
#' most_recent_season("2022-09-01") # up to september, will be recorded as 2022
#'
#' most_recent_season("2022-10-01") # then will swap over to 2023 when we hit october
#' }
#'
#' @export
most_recent_season <- function(date = Sys.Date()) {
  date <- as.Date(date)
  if (as.integer(format(date, "%m")) < 10) {
    return(as.integer(format(date, "%Y")))
  } else {
    return(as.integer(format(date, "%Y")) + 1)
  }
}

#' helper function to ensure that seasons passed into load_* functions are available
#'
#' @description The data coverage for years of `load_*()` functions can be wildly variable. To handle checking available years of data, this internal function will throw an error if the values passed into `seasons` do not fall between the first and second arguments.
#'
#' @return \value{none}, or an error if any values in `seasons` are outside of `first_season:last_season`
#' @noRd
#' @keywords internal
check_seasons_legit <- function(
  seasons,
  first_season = 2003,
  last_season = most_recent_season()
) {
  if (isTRUE(seasons)) {
    seasons <- first_season:last_season
  }
  stopifnot(
    is.numeric(seasons),
    seasons >= first_season,
    seasons <= last_season
  )
  return(seasons)
}
