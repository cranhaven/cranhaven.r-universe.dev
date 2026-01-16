#' Hourly PM25 Measurements in Taiwan
#'
#' Hourly measurements of \eqn{PM_{25}} at 15 monitoring stations from southern part of Taiwan
#' from January 1, 2006 to December 31, 2015. The first two columns are
#' the date and the hour. Missing values are filled using fixed window around the missing values.
#' Data of February 29 are removed so that there are 87600 observations in total.
#'
#' @docType data
#'
#' @usage data(TaiwanPM25)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#' @source \href{https://airtw.epa.gov.tw/}{https://airtw.epa.gov.tw/}
#'
"TaiwanPM25"
