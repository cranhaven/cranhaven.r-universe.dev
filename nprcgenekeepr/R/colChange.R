#' colChange internal function to describe column names transformation
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Description of column name changes
#'
#' @param orgCols character vector with column names to be transformed if
#' needed.
#' @param cols character vector with transformed column names
## ##  get_and_or_list
#' @importFrom stringi stri_c
#' @noRd
colChange <- function(orgCols, cols) {
  desc <- stri_c(
    get_and_or_list(orgCols[!orgCols %in% cols]), " to ",
    get_and_or_list(cols[!orgCols %in% cols])
  )
  if (desc == " to ") {
    desc <- character(0L)
  }
  desc
}
