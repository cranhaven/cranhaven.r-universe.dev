
#' Peers' party affiliations
#'
#' Calls the API to return a tibble with details on the number of Lords
#' and their party affiliations. Defaults to the current date, but can
#' also return the number of Lords and their affiliations on a given date.
#' @param date Accepts character values in `'YYYY-MM-DD'` format,
#' and objects of class `Date`, `POSIXt`, `POSIXct`,
#' `POSIXlt` or anything else than can be coerced to a date with
#' `as.Date()`. The API will return data on the composition of the
#' House of Lords on that date. Defaults to the current system date.
#' @inheritParams mnis_basic_details
#' @return A tibble with information on the numbers of different types
#' of Lords on a given date.
#' @export
#' @examples
#' \dontrun{
#' x <- mnis_lords_type()
#' }
#'
mnis_lords_type <- function(date = Sys.Date(), tidy = TRUE,
                            tidy_style = "snake_case") {

  query <- paste0(base_url, "LordsByType/", as.Date(date), "/")

  got <- mnis_query(query)

  x <- tibble::as_tibble(as.data.frame(got$LordsByType))

  if (tidy == TRUE) {
    x <- mnis::mnis_tidy(x, tidy_style)
  }
    x
}
