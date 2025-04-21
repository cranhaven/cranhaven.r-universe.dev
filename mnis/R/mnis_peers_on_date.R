
#'
#' Peers on date
#'
#' Requests data on all peers who were members of the House of Lords on the
#' date specified, (if only `date1` is included as a parameter), or on
#' or between the two dates if both `date1` and `date2` are
#' specified. Either `date1` or `date2` can be the latter of the
#' two dates.
#' @param date1 The date to return the list of peers from. Defaults to current
#' system date. Accepts character values in `'YYYY-MM-DD'` format, and
#' objects of class `Date`, `POSIXt`, `POSIXct`, `POSIXlt`
#' or anything else than can be coerced to a date with `as.Date()`.
#' @param date2 An optional query parameter. Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`,
#' `POSIXct`, `POSIXlt` or anything else than can be coerced to a
#' date with `as.Date()`. If not `NULL`, the function returns a
#' list of all peers in the House of Lords between `date1` and
#' `date2`. Defaults to `NULL`.
#' @inheritParams mnis_basic_details
#' @return A tibble with information on all peers who were members of the
#' House of Lords on the date specified, (if only `date1` is included
#' as a parameter), or on or between the two dates if both `date1`
#' and `date2` are specified.
#' @export
#' @seealso [mnis_party_state()]
#' @seealso [mnis_peers_on_date()]
#' @examples
#' \dontrun{
#' x <- mnis_peers_on_date(date1 = "2017-01-01")
#' }
#'
mnis_peers_on_date <- function(date1 = Sys.Date(), date2 = NULL,
                               tidy = TRUE, tidy_style = "snake_case") {

  date1 <- as.Date(date1)

  if (is.null(date2) == FALSE) {
    date2 <- as.Date(date2)
  }

  if (is.null(date2) == TRUE) {
    date2 <- date1
  } else if (date1 > date2) {
    date3 <- date1
    date1 <- date2
    date2 <- date3
    rm(date3)
  }

  query <- paste0(base_url, "members/query/House=Lords|Membership=all|",
                  "lordsmemberbetween=", date1, "and", date2, "/")

  got <- mnis_query(query)

  lords <- got$Members$Member

  lords <- tibble::as_tibble(lords)

  if (tidy == TRUE) {
    lords <- mnis::mnis_tidy(lords, tidy_style)
  }
    lords
}
