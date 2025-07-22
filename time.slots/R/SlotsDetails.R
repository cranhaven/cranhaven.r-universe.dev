#' Construct SlotsDetails value object
#'
#' @param start_datetime (`POSIXct`) Is the start of the time slot.For example "2022-03-16 10:00:00".
#' @param end_datetime (`POSIXct`) Is the end of the time slot.For example "2022-03-16 11:30:00".
#' @param title (`character`) Is the title of the time slot. For example "News 1".
#' @param body (`list`) Is a nested list with name value pairs of the time slot details. For example list(cost = "$1200", TARP = 5).
#'
#' @return (`data.frame`) Is a long dataframe of time slots.
#' @export
#'
#' @examples
#' SlotsDetails()
#'
SlotsDetails <- function(start_datetime = NA, end_datetime = NA, title = NA_character_, body = list(NA_character_)) {
  res <- data.frame(start_datetime, end_datetime, title)
  res$body <- body
  res <- res[stats::complete.cases(dplyr::select(res, -"body")), ]
  return(res)
}
