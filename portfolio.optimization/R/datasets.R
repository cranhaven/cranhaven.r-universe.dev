#' S&P 100 weekly stock returns 2017
#'
#' A dataset sp100w17 containing the (crude) weekly returns of (almost) all 
#' S&P 100 stocks of 2017, daily basis (101 stocks, 251 returns). 
#' 
#' Furthermore contains a vector sp100w17av with the average trading volume
#' of all stocks in 2017 - to be used for a subselection.
#'
#' @docType data
#' @keywords datasets
#' @name sp100w17
#' @usage data(sp100w17)
#' @format One xts time series object with 251 rows and 101 columns.
NULL

#' S&P 100 average trading volume over the whole year 2017
#' 
#' A vector sp100w17av with the average trading volume
#' of all stocks in 2017 - to be used e.g. for a subselection.
#'
#' @docType data
#' @keywords datasets
#' @name sp100w17av
#' @usage data(sp100w17)
#' @format One named numeric vector of length 101.
NULL

#' S&P 100 weekly stock returns 2017 of 30 stocks with the highest average
#' trading volume over the whole year
#'
#' A sceario sp100w17 containing the (crude) weekly returns of (almost) all 
#' S&P 100 stocks of 2017, daily basis (101 stocks, 251 returns). 
#'
#' @docType data
#' @keywords datasets
#' @name sp100w17av30s
#' @aliases scenario.set
#' @usage data(sp100w17av30s)
#' @format A named matrix scenario.set with 251 rows and 30 columns.
NULL
