#' Simulated 2x2 factorial trial data
#'
#' A dataset mimicking data that might be obtained from a
#' two-by-two factorial clinical trial.  It is the first 100 rows
#' of the data set \code{simdata}.
#'
#' \itemize{
#'   \item \code{subjno}  subject ID
#'   \item \code{time}  time to event or censoring
#'   \item \code{event} event indicator (1=event, 0=censoring)
#'   \item \code{indA} indicator of receiving treatment A (1=yes, 0=no)
#'   \item \code{indB} indicator of receiving treatment B (1=yes, 0=no)
#'   \item \code{cvd}  history of cardiovascular disease (1=yes, 0=no)
#'   \item \code{fac2-fac5} indicator variables for the 5 level factor variable
#' }
#'
#' @docType data
#' @keywords datasets
#' @name simdataSub
#' @usage data(simdataSub)
#' @format A data frame with 100 rows and 10 variables
NULL
