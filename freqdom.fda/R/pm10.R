#' @title PM10 dataset
#' @description 
#' Concentration of partical matter of diameter 10 micrometers or smaller (\code{PM10}) 
#' is one of the most widely adopted measurements for assesment of ambient air quality.
#' In this dataset, we provide \eqn{175} measurement of daily temporal distribution of \code{PM10}
#' in Graz, Austria, collected between December 2004 and June 2005. 
#' 
#' For the purpose of this R package, raw data of 48 observations per day, was transformed
#' to functional objects in Fourier basis using \pkg{fda} package.
#' 
#' @references Hormann, Siegfried, Brigitte Pfeiler, and Ernst Stadlober.
#' \emph{Analysis and prediction of particulate matter PM10 for the winter season in Graz.}
#' Austrian Journal of Statistics 34.4 (2005): 307-326.
#' 
#' @format \eqn{175} daily distribution functions in the \code{fd} format
#' from the \pkg{fda} package
#' @source Styrian Government, FA 17C Technical Environmental Protection and Safety, Air Quality Control Section, 
#' @examples
#' data(pm10)
#' summary(pm10)
#' plot(pm10)
"pm10"
