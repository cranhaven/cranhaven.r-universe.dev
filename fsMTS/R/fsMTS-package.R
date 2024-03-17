#' Feature selection for Multivariate Time Series
#'
#' Implementation of feature selection methods for multivariate time series
#' @name fsMTS-package
#' @docType package
#' @title Feature selection for Multivariate Time Series
#' @author Dmitry Pavlyuk \email{Dmitry.V.Pavlyuk@@gmail.com}
NULL

#' Urban traffic (pre-processed)
#'
#' The \code{fsMTS} package includes the dataset \code{traffic} that
#' contains information from 30 sensors deployed on arterial roads for one day
#' with 5-minute temporal aggregation (288 observations)
#'
#'
#' @name traffic
#' @rdname traffic
#' @docType data
#' @usage data(traffic)
#' @format A dataframe with 288 observations of a 30-dimensional time series
NULL

#' Urban traffic (preprocessed and reduced)
#'
#' The dataset \code{traffic.mini} is a reduced data set from 3 sensors deployed on arterial roads for 12 hours
#' with 5-minute temporal aggregation (144 observations)
#'
#'
#' @name traffic.mini
#' @rdname traffic.mini
#' @docType data
#' @usage data(traffic.mini)
#' @format A dataframe with 144 observations of a 3-dimensional time series
NULL
