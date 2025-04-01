
#' Calculation of the Log-likelihood assuming Identially, Independenzly and Normally Distributed errors
#' @description Calculates the i-th log-likelihood of each \emph{y-yhat} pair as described in \insertCite{Seber.2004}{spsh}.
#' @param y A vector of \code{n} observed properties/variables of interest.
#' @param yhat A vector of \code{n} model simulated properties/variables of interest.
#' @param sigma A vector of length 1 considering homoscedastic residuals.
#'
#' @details The underlying assumption is, that the model residuals (errors) are independently, and identically distributed (i.i.d.) following a normal distribution.
#' Alternatively consider using \link[=base]{dnorm}.
#' @note The assumption of i.i.d. and normal distribution is best investigated \emph{a posteriori}.
#' @return \emph{log-likelihood} value of an normal distribution with N~(0, \emph{sigma^2})
#' @references \insertRef{Seber.2004}{spsh}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' # homoscedastic residuals
#' sig.s  <- .01
#' y.scat <- rnorm(100, 0, sig.s)
#' yhat   <- (1:100)^1.2
#' y      <- yhat + y.scat
#' sum(logLikFun.norm(y, yhat, sig.s))
#' plot(yhat-y)
#' @export
logLikFun.norm <- function(y, yhat, sigma){
          
          #
          #### ARGUMENTS
          #
          #   y       num     vector of observed quantity
          #   yhat    num     vector of model predicited quantity
          #   sigma   num     standard deviation of the model residuals (y-yhat)
          #
          
          #
          #### PURPOSE
          #
          #   calculate the log-Likelihood value 
          #
      
          #### ASSUMPTIONS
          #
          #   1)  Residuals follow a normal distribution and are standardized to N~(0,1^2)
          #   2)  residuals are independent
          #
          #   Function can account heteroscedastic and homoscedastic residuals
          #   sigma   num     standard deviation of the model residuals (y-yhat)
          #
          
          
          #
          #### RETURNS
          #
          #   loglik_norm   num   skalar log-Likelihood value 
          
          N <- length(y)
          
          eta <- (y-yhat)/sigma
          
          loglik_norm_sum <- (-N/2*log(2*pi) -N*log(sigma)- 1/2 * sum(eta^2))
          
          return(loglik_norm_sum)
          
}
