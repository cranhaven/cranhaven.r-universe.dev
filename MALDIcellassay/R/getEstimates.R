#' Convert numeric vector into proportions
#'
#' @param x Numeric vector
#'
#' @return
#' Numeric vector in proportions (0 - 1)
#' @noRd
.absToProp <- function(x) {
  prop <- (x - min(x)) / (max(x) - min(x))

  return(prop)
}

#' Get value of curve for a give target effect size (e.g. 50%)
#'
#' @param model   `nplr::nplr` object
#' @param target  Numeric, typically 0.5 for the EC50 or IC50
#'
#' @return
#' Numeric, value on the fit curve.
#' Used to provide the correct target for `nplr::getEstimates()`
#' @importFrom nplr getYcurve
#' @noRd
.getFitValue <- function(model, target) {
  x <- getYcurve(model)
  prop <- .absToProp(x)
  idx <- which.min(abs(prop - target))

  return(x[idx])
}

#' Get concentration leading to effect size
#'
#' @param model   `nplr::nplr` object
#' @param target  Numeric, typically 0.5 for the EC50 or IC50
#'
#' @return
#' Numeric, pICtarget/pECtraget value
#' @importFrom nplr getEstimates
#' @noRd
.getEstimates <- function(model, target) {
  -suppressMessages(
    suppressWarnings(
      getEstimates(model,
                   targets = .getFitValue(model, target)))[, 3]
  )
}
