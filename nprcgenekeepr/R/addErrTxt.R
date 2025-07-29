#' Concatenates any errors from nprcgenekeeprErr into narrative form
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr

#' @return Error from nprcgenekeepr
#'
#' @param txt character string with initial error description value
#' @param err ve from errorLst
#' @param singularTxt character string with text used when the
#' length of err is 1
#' @param pluralTxt character string with text used when the
#' length of err is greater than 1.
#' @importFrom stringi stri_c stri_detect_fixed
#' @noRd
addErrTxt <- function(txt, err, singularTxt, pluralTxt) {
  if (length(err) == 1L) {
    if (stri_detect_fixed(err, "and")) {
      txt <- stri_c(txt, pluralTxt, ": ", err, ".\n")
    } else {
      txt <- stri_c(txt, singularTxt, ": ", err, ".\n")
    }
  } else if (length(err) > 1L) {
    if (length(err) > 5L) {
      err <- err[1L:5L]
    }
    txt <- stri_c(
      txt, pluralTxt, ": ",
      get_and_or_list(err), ".\n"
    )
  }
  txt
}
