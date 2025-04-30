#' Covariance Allocation Principle
#'
#'
#' @description
#' \loadmathjax{}
#'  This function implements the covariance allocation principle for optimal capital allocation.
#'
#' @param Loss A matrix containing the individual losses in each column
#' @param Capital A scalar representing the capital to be allocated to each loss.
#'
#' @details
#' The Covariance Allocation Principle correspond to the following expression:
#'
#' \mjtdeqn{K_{i} = \frac{K}{Var[S]} Cov(X_{i}, S), \quad i=1, \ldots, n,}{K_{i} = \dfrac{K}{Var[S]} Cov(X_{i}, S), \quad i=1, \ldots, n,}{}
#'
#' where \mjteqn{K_i}{K_i}{K_i}{}  is the capital to be allocated to the \emph{ith} loss, \mjteqn{K}{K}{} is the total capital to be allocated, \mjteqn{X_i}{X_i}{} is the individual unit loss and \emph{S} is the total (aggretate) loss, this comes from \mjteqn{\sum_{i}X_{i}}{\sum_{i}X_{i}}{}. \mjteqn{Cov(X_{i}, S)}{Cov(X_{i}, S)}{} is the covariance between the individual loss \mjteqn{X_i}{X_i}{} and the aggregate loss \emph{S}; and \mjteqn{Var(S)}{Var(S)}{} is the variance of the aggregate loss.
#'
#'
#' @return
#' A vector containing each asset and the corresponding capital allocation. If \code{Capital=1}, then the returned value will be the proportions  of capital required by each loss to be faced.
#'
#' @references
#' Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.
#'
#' Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.
#'
#' Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.
#'
#' @author Jilber Urbina
#' @noMd
#' @export
#' @importFrom mathjaxr preview_rd
#' @examples
#' data(dat1, dat2)
#'Loss <- cbind(Loss1=dat1[1:400, ], Loss2=unname(dat2))
#' # Proportions of capital to be allocated to each bussines unit
#'cap(Loss, Capital=1)
#'
#'# Capital allocation,
#'# capital is determined as the empirical VaR of the losses at 99\%
#'K <- quantile(rowSums(Loss),  probs = 0.99)
#'cap(Loss, Capital=K)


cap <-
function(Loss, Capital){
  K <- as.numeric(Capital)
  L <- as.matrix(Loss)
  S <- rowSums(Loss)
  Ki <- (K/var(S)) * apply(L, 2, function(x) cov(x, S))
  return(Ki)
}
