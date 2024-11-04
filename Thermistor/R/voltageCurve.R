#' @title Calculate V-DeltaT Curve for Given Indices
#' @description Calculating the temperature-change-voltage curve for a
#'    particular set of indices for each component corresponding to the
#'    values from a standard components space.
#' @param Tdata a vector of temperature-change values
#' @param x a vector of the indices of each component
#' @param StdRes a vector of available resistor values
#' @param StdTherm_Val a vector of available nominal thermistor resistances
#' @param StdTherm_Beta a vector of of thermistor temperature coefficients
#'
#' @return the voltage values at Point B
#' @export
#'
#' @examples
#' data(CompValues)
#' Tdata <- seq(-40, 85, by=5)
#' R_id <- c(2, 1, 4, 2, 1, 3)
#' Res <- CompValues$Res
#' ThVal <- CompValues$ThVal
#' ThBeta <- CompValues$ThBeta
#' voltageCurve(Tdata, R_id, Res, ThVal, ThBeta)
voltageCurve <- function(Tdata, x, StdRes, StdTherm_Val, StdTherm_Beta){

  y <- numeric()
  y[1] = StdRes[x[1]]
  y[2] = StdRes[x[2]]
  y[3] = StdRes[x[3]]
  y[4] = StdRes[x[4]]
  y[5] = StdTherm_Val[x[5]]
  y[6] = StdTherm_Beta[x[5]]
  y[7] = StdTherm_Val[x[6]]
  y[8] = StdTherm_Beta[x[6]]

  Vdata <- tempCompCurve(y, Tdata);
  return(Vdata)
}

