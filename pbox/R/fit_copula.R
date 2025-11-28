##############################################################
#' Copula Fit
#'
#' Internal method to automatically find the best Copula given a data.frame. Wrapper around the function \code{fitCopula}.
#'
#' @name fit_copula
#' @docType methods
#' @export
#' @include pbox.R
#' @aliases .fit_copula
#' @usage .fit_copula(copula, family, dim, u)
#' @param copula A \code{data.frame} or \code{data.table} (the data will be coerced to a \code{data.table} internally).
#' @param family List of copula types and their corresponding families. Currently supported families are "clayton", "frank", "amh", "gumbel", and "joe" for Archimedean Copula; "galambos", "gumbel", and "huslerReiss" for Extreme-Value copula; "normal" and "t" for Elliptical copula.
#' @param dim number of columns of data.
#' @param u matrix of (pseudo-)observations. Consider applying the function \code{pobs()} first in order to obtain such data.

#' @return A \code{data.table} with the corresponding AIC and the parameter estimates of the evaluated copulas and families.
#'
#'
#' @importFrom copula fitCopula coef
#' @importFrom data.table data.table
#' @importFrom stats AIC
#' @importFrom utils getFromNamespace

setGeneric(".fit_copula",
           def = function(copula, family, dim, u) {
             standardGeneric(".fit_copula")
           })
#'
#' @rdname fit_copula
#' @description
#' Automatically fits a copula model using the provided pseudo-observations.
#' This method supports various families of copulas and calculates the corresponding AIC
#' and parameter estimates.
#'
#'
setMethod(".fit_copula",
          definition=function(copula, family, dim, u) {
  if (dim > 2 && family %in% c("amh", "galambos", "huslerReiss", "tawn", "tev")) {
    return(NULL)  # Skip if the family is not available for dim > 2
  }
  copFun <-utils::getFromNamespace(copula,ns = "copula")
  cop <- copFun(family = family, param = NA_real_, dim = dim)


  fit <- copula::fitCopula(cop, u, method = "ml")
  aicVal <- stats::AIC(fit)
  coefVal <- copula::coef(fit)

  return(data.table::data.table(copula=copula,family = family, AIC = aicVal, coef = coefVal))
}
       )



