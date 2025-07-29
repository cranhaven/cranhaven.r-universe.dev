#' Get Indian-origin status of group
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return A list with a list named \code{ancestry} having named integer values
#'         corresponding to the origin types and the count of each type
#'         found in the function argument \code{origin}, the \code{color}
#'         used for the heat map where "red" indicates "Chinese" or "Hybrid"
#'         origin, "yellow" indicates "Borderline", and "green" otherwise; and
#'         the \code{color_index) values are \code{1}, \code{2}, or \code{3}
#'         corresponding to "red", "yellow", and "green".
#' @param origin character vector of the animal origins. This vector
#' is to have already been filtered to remove animals that should not be
#' included in the calculation.
#' @importFrom stringi stri_startswith_fixed stri_detect_fixed
#' @noRd
getIndianOriginStatus <- function(origin) {
  chinese <- length(origin[stri_detect_fixed(origin, "CHINESE")])
  indian <- length(origin[stri_detect_fixed(origin, "INDIAN")])
  hybrid <- length(origin[stri_startswith_fixed(origin, "HYBRID")])
  borderline <- length(origin[stri_detect_fixed(origin, "BORDERLINE_HYBRID")])
  japanese <- length(origin[stri_detect_fixed(origin, "JAPANESE")])
  unknown <- length(origin[stri_detect_fixed(origin, "UNKNOWN")])
  other <- length(origin[stri_detect_fixed(origin, "OTHER")])

  ancestry <- list(
    chinese = chinese,
    indian = indian,
    hybrid = hybrid,
    borderline = borderline,
    japanese = japanese,
    unknown = unknown,
    other = other
  )
  if ((chinese + hybrid) >= 1L) {
    color <- "red"
    colorIndex <- 1L
  } else if (borderline >= 1L) {
    color <- "yellow"
    colorIndex <- 2L
  } else {
    color <- "green"
    colorIndex <- 3L
  }
  list(ancestry = ancestry, color = color, colorIndex = colorIndex)
}
