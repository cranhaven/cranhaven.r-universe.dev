
#' Party standing on a given date
#'
#' Returns data on the numbers and gender of MPs, by party, for the given
#' date, in either the House of Commons or the House of Lords.
#' @param house The house of parliament. Accepts either `'Lords'` or
#' `'Commons'`. This parameter is not case sensitive, so both
#' `'lords'` and `'Lords'` return the same result.
#' Defaults to `'Commons'`.
#' @param date The date to query party standing on. Accepts character values
#' in `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can
#' be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' @inheritParams mnis_basic_details
#' @return A tibble with information on the total numbers and gender of
#' MPs, by party, for the given date and house.
#' @seealso [mnis_mps_on_date()]
#' @seealso [mnis_peers_on_date()]
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_party_state("2012-01-12")
#' }
#'
mnis_party_state <- function(house = "Commons", date = Sys.Date(),
                             tidy = TRUE, tidy_style = "snake_case") {
  query <- paste0(base_url, "houseOverview/", house, "/", date, "/")

  got <- mnis_query(query)

  x <- tibble::as_tibble(got$HouseOverview$Party)

  if (tidy == TRUE) {
    x <- mnis_tidy(x, tidy_style)
  } else {
    x
  }
}
