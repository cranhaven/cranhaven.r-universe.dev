#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @usage str_detect_fixed_all(strings, patterns, ignore_na, ...)
#' @return logical vector with results of stri_detect() for each pattern in
#' the character vector in the parameter \code{patterns}.
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See fixed, ignore.case and perl sections
#' for details. See  \emph{Extended Regular Expressions} for how to use regular
#' expressions for matching.
#' @param ignore_na if TRUE NA values are trimmed out of \code{strings} and
#' \code{patterns} before comparison
#' @param ... further arguments for stri_detect_fixed
#' @importFrom stringi stri_detect_fixed
#' @noRd
str_detect_fixed_all <- function(strings, patterns, ignore_na = FALSE, ...) {
  if (ignore_na) {
    strings <- strings[!is.na(strings)]
    patterns <- patterns[!is.na(patterns)]
  }
  vapply(patterns, function(pattern) {
    any(stri_detect_fixed(strings, pattern, ...))
  }, logical(1L))
}
