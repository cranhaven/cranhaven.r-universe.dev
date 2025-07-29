#' Returns a character vector with an file name having the date prepended.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A character string with a file name prepended with the date and time
#' in YYYY-MM-DD_hh_mm_ss_basename format.
#'
#' @param filename character vector with name to use in file name
#' @importFrom lubridate now
#' @importFrom stringi stri_c stri_replace_all_fixed
#' @export
#' @examples
#' library(nprcgenekeepr)
#' getDatedFilename("testName")
getDatedFilename <- function(filename) {
  dateStamp <- stri_replace_all_fixed(
    stri_replace_all_fixed(as.character(now()), " ", "_"), ":", "_"
  )
  stri_c(dateStamp, "_", filename)
}
