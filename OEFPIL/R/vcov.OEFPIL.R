#' @name vcov.OEFPIL
#' @title Covariance matrix from an OEFPIL object
#' @description Function for extracting the estimated covariance matrix from an object of class \code{"OEFPIL"}.
#'
#' @param object an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param ...    other arguments.
#'
#' @return A matrix of the estimated covariances between the parameter estimates from an \code{"OEFPIL"} object.
#'
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' ##Use of vcov function
#' vcov(st1)
#'
#' @method vcov OEFPIL
#' @export
vcov.OEFPIL <- function(object,...) {
  ## Function for extracting the estimated covariance matrix from 'OEFPIL' object.

  return(object$cov.m_Est)
}
