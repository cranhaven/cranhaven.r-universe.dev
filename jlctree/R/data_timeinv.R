#' A simulated dataset with time-invariant longitudinal outcome and covariates.
#'
#' A simulated dataset with time-invariant longitudinal outcome,
#' time-to-event, and time-invariant covariates.
#' Since longitudinal outcome and all of the covariates are time-invariant, 
#' there is only one observation per subject. 
#' The time-to-event data is right-censored.
#'
#' @format A data frame with 500 rows and 10 variables.
#' \describe{
#' \item{ID}{subject identifier (1 - 500)}
#' \item{X1}{continuous covariate between 0 and 1; time-invariant}
#' \item{X2}{continuous covariate between 0 and 1; time-invariant}
#' \item{X3}{binary covariate; time-invariant}
#' \item{X4}{continuous covariate between 0 and 1; time-invariant}
#' \item{X5}{categorical covariate taking values from {1, 2, 3, 4, 5}; time-invariant}
#' \item{time_Y}{right-censored event time}
#' \item{delta}{censoring indicator, 1 if censored and 0 otherwise}
#' \item{y}{longitudinal outcome; time-invariant}
#' \item{g}{true latent class identifier {1, 2, 3, 4}, which is determined by 
#'  the outcomes of \eqn{1\{X1 > 0.5\}} and \eqn{1\{X2 > 0.5\}}, with some noise}
#'  }
#'
#' @examples 
#' # The data for the first five subjects (ID = 1 - 5):
#' #
#' #  ID   X1   X2 X3  X4 X5    time_Y delta         y g
#' #   1 0.27 0.53  1 0.8  1 10.703940     0 0.8923776 2
#' #   2 0.37 0.68  1 0.5  3  9.153915     1 0.6871529 2
#' #   3 0.57 0.38  1 0.2  1  4.489658     1 0.8410745 3
#' #   4 0.91 0.95  0 0.4  3  1.009941     1 2.1058681 4
#' #   5 0.20 0.12  0 0.8  5 11.125094     0 0.1383508 1
#'
#' @docType data
#' @keywords data
#' @name data_timeinv
#' @usage data(data_timeinv)
NULL



