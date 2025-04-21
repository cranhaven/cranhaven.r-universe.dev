
#' All MPs between two dates
#'
#' Requests data on all MPs who were members of the House of Commons on the
#' date specified, (if only `date1` is included as a parameter), or
#' on or between the two dates if both `date1` and `date2` are
#' specified. Either `date1` or `date2` can be the latter of
#' the two dates.
#' @param date1 The date to return the list of mps from. Accepts character
#' values in `'YYYY-MM-DD'` format, and objects of class `Date`,
#' `POSIXt`, `POSIXct`, `POSIXlt` or anything else than can
#' be coerced to a date with `as.Date()`. Defaults to current system date.
#' @param date2 An optional query parameter. Accepts character values in
#' `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`,
#' `POSIXct`, `POSIXlt` or anything else than can be coerced to a date
#' with `as.Date()`. If not `NULL`, the function returns a list of
#' all MPs who were members between `date1` and `date2`.
#' Defaults to `NULL`.
#' @inheritParams mnis_basic_details
#'
#' @return A tibble with information on all MPs who were members of the
#' House of Commons on the date specificed (if only `date1` is included
#' as a parameter), or on or between the two dates if both `date1` and
#' `date2` are specified.
#' @export
#' @seealso [mnis_party_state()]
#' @seealso [mnis_peers_on_date()]
#'
#' @examples
#' \dontrun{
#' x <- mnis_mps_on_date(date1 = "2017-01-01", date2 = "2014-02-04")
#' }
#'
mnis_mps_on_date <- function(date1 = Sys.Date(), date2 = NULL,
                             tidy = TRUE, tidy_style = "snake_case") {

  date1 <- as.Date(date1)

  if (is.null(date2) == FALSE) {
    date2 <- as.Date(date2)
  }

  if (is.null(date2) == TRUE) {
    date2 <- date1
  }

  date_vec <- c(date1, date2)

  query <- paste0(base_url, "members/query/House=Commons|Membership=all|",
                  "commonsmemberbetween=", min(date_vec), "and",
                  max(date_vec), "/")

  got <- mnis_query(query)

  mps <- got$Members$Member

  mps <- tibble::as_tibble(mps)

  if (.Platform$OS.type == "windows") {
    mps$MemberFrom <- stringi::stri_trans_general(mps$MemberFrom, "latin-ascii")

    mps$MemberFrom <- gsub("Ynys MA\U00B4n", "Ynys M\U00F4n", mps$MemberFrom)
  }

  if (tidy == TRUE) {
    mps <- mnis::mnis_tidy(mps, tidy_style)
  }
    mps
}
