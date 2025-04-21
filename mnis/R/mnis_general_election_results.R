
#' Election results
#'
#' Returns an object containing list with details of the search parameter and
#' a tibble with election results. Accepts queries on location type and name,
#' and the start and end date to return general elections between. The API does
#' not contain data for Norther Ireland.
#'
#' @param location_type The type of area to return information for. Accepts
#' `'Country'`, `'Region'`, `'County'`, and
#' `'Constituency'`. Defaults to `'Country'`.
#' @param location_name The location to return data for. It can be the name
#' of any Country, Region, County or Constituency. Defaults to 'Great Britain'.
#' @param start_date Start date of search. Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can be
#' coerced to a date with `as.Date()`. Defaults to `'1900-01-01'`.
#' @param end_date End date of search. Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can
#' be coerced to a date with `as.Date()`. Defaults to current system date.
#' @inheritParams mnis_basic_details
#' @return Returns a list with details of the search parameter and
#' a tibble with election results.
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_general_election_results(
#'   location_type = "Country", location_name = "England",
#'   start_date = "2010-01-01", end_date = "2016-01-01"
#' )
#' }
#'
mnis_general_election_results <- function(location_type = "Country",
                                          location_name = "Great Britain",
                                          start_date = "1900-01-01",
                                          end_date = Sys.Date(),
                                          tidy = TRUE,
                                          tidy_style = "snake_case") {
  query <- paste0(base_url, "GeneralElectionResults/", location_type, "/",
                  utils::URLencode(location_name), "/", as.Date(start_date),
                  "/", as.Date(end_date), "/")

  got <- mnis_query(query)

  x <- got$ElectionResults

  x$ElectionResult <- as_tibble(x$ElectionResult)

  if (tidy == TRUE) {
    names(x)[names(x) == "LocationInfo"] <- "location_info"

    names(x)[names(x) == "ElectionResult"] <- "election_result"

    x$election_result <- mnis::mnis_tidy(x$election_result, tidy_style)
  }
    x
}
