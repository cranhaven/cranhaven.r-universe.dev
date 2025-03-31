#' @title Meteorological data for feature selection analysis
#'
#' @description A dataset with meteorological data from
#'  a weather station in Frankfurt (Oder), Germany from
#'  february 2016
#'
#' @name efsdata
#' @docType data
#'
#' @usage data(efsdata)
#'
#' @format a data frame with 29 entries and following 7
#'  variables
#'\describe{
#'  \item{\code{date}}{index variable from 1 to 29}
#'  \item{\code{Tmin}}{temperature minimum of the day}
#'  \item{\code{Tmax}}{temperature maximum of the day}
#'  \item{\code{SunAvg}}{sunshine duration of the day}
#'  \item{\code{RainBool}}{classification variable: if
#'    it has not rained: 0, if it has rained: 1}
#'  \item{\code{RelHumAvg}}{average relative humidity
#'    of the day}
#'  \item{\code{WindForceAvg}}{average wind force of
#'    the day}
#'}
#' @keywords datasets
#'
#' @references modified data from
#'  \url{http://wetterstationen.meteomedia.de/}
#'
#'
#'
NULL
