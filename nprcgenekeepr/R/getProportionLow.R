#' Get proportion of Low genetic value animals
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return List of the proportion of Low genetic value animals and the
#' dashboard color to be assigned base on that proportion.
#'
#' @param geneticValues character vector of the genetic values. This vector
#' is to have already been filtered to remove animals that should not be
#' included in the calculation.
#' @importFrom stringi stri_detect_fixed
#' @noRd
getProportionLow <- function(geneticValues) {
  proportion <-
    length(geneticValues[stri_detect_fixed(geneticValues, "Low")]) /
      length(geneticValues)
  if (proportion > 0.5) {
    color <- "red"
    colorIndex <- 1L
  } else if (proportion <= 0.5 && proportion >= 0.3) {
    color <- "yellow"
    colorIndex <- 2L
  } else if (proportion < 0.3) {
    color <- "green"
    colorIndex <- 3L
  }
  list(proportion = proportion, color = color, colorIndex = colorIndex)
}
