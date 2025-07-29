#' Returns the elapsed time since start_time.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Taken from github.com/rmsharp/rmsutilityr
#'
#' @return A character vector describing the passage of time in hours, minutes,
#' and seconds.
#'
#' @param start_time a POSIXct time object
#' @importFrom stringi stri_c
#' @export
#' @examples
#' start_time <- proc.time()
#' ## do something
#' elapsed_time <- get_elapsed_time_str(start_time)
get_elapsed_time_str <- function(start_time) {
  # To use: collect the start_time at the beginning of the script with
  # start_time <- proc.time()
  # At the end call this function using start_time as the sole argument
  # elapsed_time <- get_elapsed_time_str(start_time)
  total_seconds <- (proc.time()[[3L]] - start_time[[3L]])
  total_minutes <- total_seconds / 60L
  hours <- floor(total_minutes / 60L)
  minutes <- floor(total_minutes - hours * 60L)
  seconds <- round(total_seconds - (hours * 3600L) - (minutes * 60L), 0L)
  hours_str <- ifelse(hours > 0L, stri_c(hours, " hours, "), "")
  minutes_str <- ifelse(minutes > 0L, stri_c(minutes, " minutes and "), "")
  seconds_str <- stri_c(seconds, " seconds.")
  stri_c(hours_str, minutes_str, seconds_str)
}
