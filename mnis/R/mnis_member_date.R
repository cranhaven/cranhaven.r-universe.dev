
#' Returns a tibble with a member's status on a given date.
#'
#' @param ID The ID of the member, or a vector with the IDs of multiple MPs.
#' If given multiple IDs, the results are combined into a single tibble.
#' Currently only accepts IDs from the default membership ID scheme. If
#' `NULL` the function stops and no data is returned.
#' Defaults to `NULL`.
#' @param date Accepts character values in `'YYYY-MM-DD'` format, and
#' objects of class `Date`, `POSIXt`, `POSIXct`,
#' `POSIXlt` or anything else than can be coerced to a date with
#' `as.Date()`. Return details on the requested member's status on
#' that date. Defaults to the current system date.
#' @inheritParams mnis_basic_details
#' @return Returns a tibble with the given member's status on the given date.
#' @keywords mnis
#' @export
#' @seealso [mnis_mps_on_date()]
#' @examples
#' \dontrun{
#' x <- mnis_member_date(172)
#' }
#'
mnis_member_date <- function(ID = NULL, date = Sys.Date(),
                             tidy = TRUE, tidy_style = "snake_case") {
  if (missing(ID)) {
    stop("The ID parameter cannot be empty, please specify a Member of Parliament or a Peer.")
  }

  query <- paste0(base_url, "member/historical/", as.character(ID),
                  "/", as.Date(date), "/")

  got <- mnis_query(query)

  x <- as.list(got$Member)

  x <- unlist(x)

  x <- t(x)

  x <- tibble::as_tibble(x)

  if (tidy == TRUE) {
    x <- mnis::mnis_tidy(x, tidy_style)
  }
    x
}
