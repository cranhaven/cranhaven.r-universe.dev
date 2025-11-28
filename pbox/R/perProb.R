#' Compute Probability Using a Perturbed Copula
#'
#' Computes the probability by applying a perturbation to the copula parameters within a 'pbox' object,
#' and then evaluating the probability for specified query values. This method ensures that variations
#' in the copula parameters can be assessed for their impact on the computed probabilities.
#'
#' @name perProb
#' @export
#' @param x A 'pbox' object representing the base copula.
#' @param vecQuery A numeric vector representing the query values.
#' @return The probability computed using a perturbed copula.
#' @examples
#'   data(SEAex)
#'   pbx <- set_pbox(SEAex[, .(Malaysia, Thailand)])
#'   vecQuery <- c(31, 34)
#'   perProb(pbx, vecQuery)
#' @importFrom data.table copy
#' @importFrom copula pMvdc
setGeneric("perProb",
           def = function(x, vecQuery) {
             standardGeneric("perProb")
           })

#' @rdname perProb
#' @description
#' `perProb` method for objects of class 'pbox'.
#' This method perturbs the parameters of the copula contained in the 'pbox'
#' and then computes the probability of the vector query using the perturbed copula.
#' The perturbation process adjusts the copula parameters and evaluates the impact
#' on the outcome probability.
#' @param x A 'pbox' object, which is expected to contain a copula.
#' @param vecQuery A numeric vector representing the query values.
#' @return Numeric value representing the computed probability using the perturbed copula.
#' @seealso \code{\link{set_pbox}}, \code{\link{pMvdc}}

setMethod("perProb",
          signature = "pbox",
          definition=function(x,vecQuery){
            if (!inherits(x, c("pbox"))) {
              stop("Input must be a pbox object!")
            }
  perCop<-copy(x)
  oMargins<-x@copula@paramMargins
  perCop@copula@paramMargins<-perturbate_params(oMargins)
  pMvdc(vecQuery,perCop@copula)
})
