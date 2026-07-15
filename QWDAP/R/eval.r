#' @title Evaluation
#' @description calculate the Coefficient of Determination, Root Mean Squared Error and the
#' Mean Absolute Error between two series.
#' @usage qwdap.eval(series1, series2)
#' @param series1 The series1.
#' @param series2 The series2.
#'
#' @return Three indicators, the Coefficient of Determination, Root Mean Squared Error and
#' Mean Absolute Error.
#' @export qwdap.eval
#'
#' @examples
#' set.seed(1)
#' res.eval <- qwdap.eval(rnorm(100,0,2),rnorm(100,0,1))
qwdap.eval <- function(series1,series2){
  rsquare <- 1 - sum((series1-series2)^2)/sum((series1-mean(series1))^2)
  rmse <- sqrt(sum((series2-series1)^2)/length(series1))
  mae <- sum(abs(series2-series1))/length(series1)
  return(data.frame("Station"="","Method"="","R2"=rsquare,"RMSE"=rmse,"MAE"=mae))
}