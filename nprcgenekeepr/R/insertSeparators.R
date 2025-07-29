#' Inserts "-" between year, month, and day of YYYYMMDD formatted dates.
#'
#' Inserts "-" between year, month, and day of a date string in dates formatted
#' as YYYYMMDD to form YYYY-MM-DD formatted dates.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' This function is not exported because it is not general purpose and
#' is missing several defensive programming measures.
#'
#' @return A character vector of potential dates in \%Y-\%m-\%d format.
#'
#' @param dates character vector of potential dates
#'
#' @importFrom stringi stri_detect_regex stri_sub stri_c
#'
#' @noRd
insertSeparators <- function(dates) {
  if (!any(stri_detect_regex(dates[!is.na(dates)], pattern = "[-/]")) &&
      all(suppressWarnings(as.integer(dates[!is.na(dates)]) &
                           !is.na(as.integer(dates[!is.na(dates)]))))) {
    dates <- vapply(dates, function(x) {
      stri_c(
        stri_sub(x, from = 1L, to = 4L),
        "-",
        stri_sub(x, from = 5L, to = 6L),
        "-",
        stri_sub(x, from = 7L, to = 8L)
      )
    }, character(1L))
  }
  dates
}
