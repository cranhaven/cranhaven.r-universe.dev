#' Returns \code{TRUE} if every member of the vector is \code{TRUE}.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Relations
#' @return \code{TRUE} if every member of the vector is \code{TRUE} else it
#' returns \code{FALSE}.
#' Considers NA values the same as false
#' @param v logical vector
#' @noRd
allTrueNoNA <- function(v) {
  # nolint start: commented_code_linter.
  # v <- all(v)
  # v <- if (is.na(v)) FALSE else v
  # return(v)
  # nolint end: commented_code_linter.
  # The following is equivalent and should be a bit faster
  all(c(v, !anyNA(v)), na.rm = TRUE)
}
