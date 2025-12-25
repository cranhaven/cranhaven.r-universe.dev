#' Time series from 35 weather stations of Canada.
#'
#' A dataset containing time series from 15 weather stations (The Pas station
#' and more 34 stations to estimate the temperature curve at the Pas station).
#' This dataset is present in the \code{fda} package.
#'
#' @usage data(canada)
#'
#' @format A list with four matrices:
#' \describe{
#'   \item{m_data}{A matrix with 14 columns where each column is a wheather
#'   station}
#'   \item{m_coord}{A matrix with 14 rows where each row is a weather station}
#'   \item{ThePas_coord}{Coordinate of the The Pas station}
#'   \item{ThePas_ts}{Observed time series of the station The Pas}
#' }
#' @source \url{https://weather.gc.ca}
#' @references J. O. Ramsay, Spencer Graves and Giles Hooker (2020). \code{fda}:
#' Functional Data Analysis. R package version 5.1.9.
#' \url{https://CRAN.R-project.org/package=fda}
"canada"
