#' Global Annual Temperature Anomalies (Land Meteorological Stations)
#'                                   (1880-2015)
#'
#' This dataset is from \url{https://cdiac.ess-dive.lbl.gov/ftp/trends/temp/hansen/gl_land.txt}.
#'
#' Global annual temperature anomalies (degrees C) computed using data from
#' land meteorological stations, 1880-2015.
#' Anomalies are relative to the 1951-1980 base period means.
#'
#' Non-computed values are indicated by "-99.99".
#'
#' @format A data frame with 136 rows and 3 columns:
#' \describe{
#'   \item{Year}{Year of observation.}
#'   \item{AnnualAnomaly}{Value in degrees C of the global annual temperature anomaly.}
#'   \item{5-YearMean}{5-Year mean of temperature anomalies.}
#' }
"tempanomalies"
