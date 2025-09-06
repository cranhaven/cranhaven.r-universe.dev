#' tranSurv:Transformation Model Based Survival Curve Estimation with Dependent Left Truncation
#'
#' A package that estimates survival curve under a dependent truncation and independent right censoring via a structural transformation method.
#' The package also includes hypothesis test of quasi-independence based on the conditional Kendall's tau of Martin and Betensky (2005) and
#' two versions of the inverse probability weighted Kendall's tau of Austin and Betensky (2014).
#'
#' @references Martin E. and Betensky R. A. (2005), Testing quasi-independence of failure and truncation times via conditional Kendall's tau,
#' \emph{Journal of the American Statistical Association}, \bold{100} (470): 484-492.
#' @references Austin, M. D. and Betensky R. A. (2014), Eliminating bias due to censoring in Kendall's tau estimators for quasi-independence of truncation and failure,
#' \emph{Computational Statistics & Data Analysis}, \bold{73}: 16-26.
#' @references Chiou, S., Austin, M., Qian, J. and Betensky R. A. (2016), Transformation model estimation of survival under dependent truncation and independent censoring,
#' \emph{Statistical Methods in Medical Research}, \bold{28} (12): 3785-3798.
#'
#' @importFrom survival Surv survfit
#' @importFrom SQUAREM squarem
#' @importFrom graphics abline legend lines par plot points title mtext text
#' @importFrom stats approx optimize pnorm uniroot coef lm
#' @importFrom utils head
#' 
#' @docType package
#' @useDynLib tranSurv, .registration = TRUE
"_PACKAGE"
NULL
