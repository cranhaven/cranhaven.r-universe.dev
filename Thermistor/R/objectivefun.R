#' @title Squared Loss of Objective
#' @description Measuring the difference between the V-DeltaT Curve you get
#'    and the target curve under squared loss.
#' @param x a vector of the indices of each component
#' @param StdRes a vector of available resistor values
#' @param StdTherm_Val a vector of available nominal thermistor resistances
#' @param StdTherm_Beta a vector of of thermistor temperature coefficients
#' @param Tdata a vector of temperature-change values
#'
#' @return the squared loss
#' @export
#'
#' @examples
#' data(CompValues)
#' Tdata <- seq(-40, 85, by=5)
#' R_id <- c(43, 36, 29, 15, 9, 3)
#' Res <- CompValues$Res
#' ThVal <- CompValues$ThVal
#' ThBeta <- CompValues$ThBeta
#' SquaredLoss(R_id, Res, ThVal, ThBeta, Tdata)
#' ### 0.04066336
SquaredLoss <- function(x, StdRes, StdTherm_Val,
                              StdTherm_Beta, Tdata){
  Vdata <- 1.026e-1 + -1.125e-4 * Tdata + 1.125e-5 * Tdata^2

  y <- numeric()
  y[1] = StdRes[x[1]]
  y[2] = StdRes[x[2]]
  y[3] = StdRes[x[3]]
  y[4] = StdRes[x[4]]
  y[5] = StdTherm_Val[x[5]]
  y[6] = StdTherm_Beta[x[5]]
  y[7] = StdTherm_Val[x[6]]
  y[8] = StdTherm_Beta[x[6]]

  Vnew <- tempCompCurve(y, Tdata)

  Residual <-  Vnew - Vdata
  G <- sum(Residual^2)

  return(G)
}
