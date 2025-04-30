#'Haircut Allocation Principle
#'
#' @description
#' \loadmathjax{}
#' Capital allocation based on the Haircut Allocation Principle.
#'
#' @param Loss Either a scalar or a vector of size \emph{N} containing the mean losses.
#' @param Capital A scalar representing the capital to be allocated to each loss.
#' @param alpha A numeric value (either a single one or a vector) consisting of the significance
#' level at which ES has to be computed, it can either be a single numeric value or a vector of
#' numeric values.
#' @param model A character string indicating which distribution is to be used for computing the
#' VaR underlying the Haircut Allocation Principle (HAP), the default value is the \code{normal}
#' distribution, the other alternative is \code{t-student} distribution with \eqn{\upsilon}{\upsilon}
#' degrees of freedom. When \code{model='both'} \code{'normal'} as well as \code{'t-student'} are used
#' when computing the HAP, see examples.
#' @param df An integer indicating the degrees of freedom for the t-student distribution when setting
#'  \code{model='t-student'} and \code{model='both'}. \code{df} must be greater than 2.
#'
#' @details
#' This function computes the capital allocation based on the so-called Haircut Allocation Principle whose expression is as follows:
#'
#'\mjtdeqn{K_{i} = \frac{K}{\sum_{j=1}^{n} F_{X_{j}}^{-1}(p)} F_{X_{i}}^{-1}(p)}{K_{i} = \frac{K}{\sum_{j=1}^{n} F_{X_{j}}^{-1}(p)} F_{X_{i}}^{-1}(p)}{}
#'
#' For \mjteqn{i=1, \ldots, n}{i=1,\dots,n}{}, where \mjteqn{K_i}{K_i}{} represents the optimal capital to be allocated to each individual loss for the \emph{i}-th business unit, \emph{K} is the total capital to be allocated, \mjteqn{F_{X_{i}}^{-1}(p)}{F_{X_{i}}^{-1}(p)}{} is the quantile function (VaR) for the \emph{i}-th loss.
#'
#' @return
#'  A vector containing the optimal capital allocation, if
#'  \code{Capital} is set to 1, then the returned matrix will consist of the proportions of capital
#'  each individual loss needs to be optimally faced.
#'
#' @references
#' Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.
#'
#'McNeil, A. J.; Frey, R. & Embrechts, P. \emph{Quantitative risk management: concepts, techniques and tools}. Princeton University Press, 2005.
#'
#'Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.
#'
#'Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.
#'
#'
#' @seealso
#' \code{\link{Overbeck2}}, \code{\link{cap}}
#'
#' @author Jilber Urbina
#' @noMd
#' @export
#' @importFrom mathjaxr preview_rd
#'@examples
#'
#'data(dat1, dat2)
#'Loss <- cbind(Loss1=dat1[1:400, ], Loss2=unname(dat2))
#'# Proportions of capital to be allocated to each bussines unit
#'hap(Loss, Capital=1)
#'
#' # Capital allocation,
#' # capital is determined as the empirical VaR of the losses at 99\%
#'K <- quantile(rowSums(Loss),  probs = 0.99)
#'hap(Loss, Capital=K)



hap <-function (Loss, Capital, alpha = 0.95, model = "normal", df = NULL)
{
  K <- as.numeric(Capital)
  Loss <- as.matrix(Loss)
  alpha <- as.numeric(alpha)

  # individual VaR
  VaR_i <- sapply(1:ncol(Loss), function(i){
    VaR(variance = var(Loss[,i]), alpha = alpha, model = model, df = df)
  })

  return((K/sum(VaR_i)) * VaR_i)
}
