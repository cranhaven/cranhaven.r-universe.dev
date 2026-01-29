#' Non-standard student-t density
#'
#' Computes the density.
#'
#' For internal use
#'
#' @param x Numeric vector. Data set to which the density is evaluated.
#' @param df Numeric constant. Degrees of freedom (> 0, maybe non-integer)
#' @param mean Numeric constant. Location parameter.
#' @param sd Positive numeric constant. Scale parameter.
dt_ <-
  function(x, df, mean, sd) {
    dt((x - mean) / sd, df, ncp = 0) / sd
  }
