

#' Members who joined between two dates.
#'
#' Function returns all members who took their seats in the house between
#' two given dates.
#' @param start_date The start date of the search, Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can
#' be coerced to a date with `as.Date()`. Defaults to `'1900-01-01'`.
#' @param end_date The end date of the search. Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can be
#' coerced to a date with `as.Date()`. Defaults to the current system date.
#' @param house The house to which the member belongs. Accepts one of
#' `'all'`, `'lords'` and `'commons'`. This parameter is
#' not case sensitive. Defaults to `'all'`.
#' @param party All members from a given party who joined between the two
#' dates. The party name must be fully spelled out (e.g. `'green party'`),
#' the API does not accept searches on this parameter. For a tibble of
#' parties, see [ref_parties()].
#' This parameter is not case sensitive. Defaults to `NULL`.
#' @param eligible If the member is currently eligible to sit. Accepts
#' one of `'all'`, `'current'`, `'former'`. This parameter
#' is not case sensitive. Defaults to `'all'`.
#' @inheritParams mnis_basic_details
#' @return A tibble with data on all members who joined
#' between the two given dates.
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_joined_between(
#'   start_date = "2015-01-01",
#'   end_date = "2017-01-01", party = "labour"
#' )
#' }
#'
mnis_joined_between <- function(start_date = "1900-01-01",
                                end_date = Sys.Date(), house = "all",
                                party = NULL, eligible = "all",
                                tidy = TRUE, tidy_style = "snake_case") {
  house <- tolower(as.character(house)) ## Making sure house works

  if (is.na(pmatch(house, c("all", "lords", "commons")))) {
    stop("Please select one of 'all', 'lords' or 'commons' for the parameter 'house'")
  }

  eligible <- tolower(as.character(eligible)) ## Making sure house works

  if (is.na(pmatch(eligible, c("all", "current", "former")))) {
    stop("Please select one of 'all', 'current' or 'former' for the parameter 'eligible'")
  }

  if (house == "lords") {
    house_query <- "|house=lords"
  } else if (house == "commons") {
    house_query <- "|house=commons"
  } else {
    house_query <- "|house=all"
  }

  if (eligible == "current") {
    eligible_query <- "|iseligible=TRUE"
  } else if (eligible == "former") {
    eligible_query <- "|iseligible=FALSE"
  } else {
    eligible_query <- NULL
  }

  if (is.null(party) == FALSE) {
    party_q <- paste0("|party*", utils::URLencode(party))
  } else {
    party_q <- NULL
  }

  query <- paste0(base_url, "/members/query/joinedbetween=",
                  as.Date(start_date), "and", as.Date(end_date), house_query,
                  party_q, eligible_query)

  got <- mnis_query(query)

  x <- tibble::as_tibble(got$Members$Member)

  if (tidy == TRUE) {
    x <- mnis::mnis_tidy(x, tidy_style)
  }
    x
}
