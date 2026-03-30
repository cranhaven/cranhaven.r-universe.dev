#' Calculating SAM priors
#'
#' This function is exactly from the SAMprior R package version 1.1.1.
#' It is included to support the runSAM function in this package.
#'
#' The \code{SAM_prior} function is designed to display the SAM prior, given
#' the informative prior (constructed from historical data), non-informative
#' prior, and the mixture weight calculated using \code{\link{SAM_weight}}
#' function (\emph{Yang, et al., 2023}).
#'
#' @param if.prior Informative prior constructed from historical data,
#' represented (approximately) as a mixture of conjugate distributions.
#' @param nf.prior Non-informative prior used for the mixture.
#' @param weight Weight assigned to the informative prior component
#' (\eqn{0 \leq} \code{weight} \eqn{\leq 1}), which should be determined by
#' \code{\link{SAM_weight}} function.
#' @param ... Additional parameters required for different endpoints.
#'
#' @details SAM prior is constructed by mixing an informative prior
#' \eqn{\pi_1(\theta)}, constructed based on historical data, with a
#' non-informative prior \eqn{\pi_0(\theta)} using the mixture weight
#' \eqn{w} determined by \code{\link{SAM_weight}} function to achieve the
#' degree of prior-data conflict (\emph{Schmidli et al., 2015, Yang et al., 2023}).
#'
#'  Let \eqn{\theta} and \eqn{\theta_h} denote the treatment effects
#'  associated with the current arm data \eqn{D} and historical data \eqn{D_h},
#'  respectively. Let \eqn{\delta} denote the clinically significant difference
#'  such that if \eqn{|\theta_h - \theta| \ge \delta}, then \eqn{\theta_h} is
#'  regarded as clinically distinct from \eqn{\theta}, and it is therefore
#'  inappropriate to borrow any information from \eqn{D_h}. Consider two
#'  hypotheses:
#'
#'  \deqn{H_0: \theta = \theta_h, ~ H_1: \theta = \theta_h + \delta ~ or ~  \theta = \theta_h - \delta.}
#'  \eqn{H_0} represents that \eqn{D_h} and \eqn{D} are consistent (i.e.,
#'  no prior-data conflict) and thus information borrowing is desirable,
#'  whereas \eqn{H_1} represents that the treatment effect of \eqn{D}
#'  differs from \eqn{D_h} to such a degree that no information should be
#'  borrowed.
#'
#'  The SAM prior uses the likelihood ratio test (LRT) statistics \eqn{R} to
#'  quantify the degree of prior-data conflict and determine the extent of
#'  information borrowing.
#'
#'  \deqn{R = P(D | H_0, \theta_h) / P(D | H_1, \theta_h) = P(D | \theta = \theta_h) / \max(P(D | \theta = \theta_h + \delta), P(D | \theta = \theta_h - \delta)) ,}
#'  where \eqn{P(D | \cdot)} denotes the likelihood function. An alternative
#'  Bayesian choice is the posterior probability ratio (PPR):
#'
#'  \deqn{R = P(D | H_0, \theta_h) / P(D | H_1, \theta_h) = P(H_0) / P( H_1) \times BF, }
#'  where \eqn{P(H_0)} and \eqn{P(H_1)} is the prior probabilities of \eqn{H_0}
#'  and \eqn{H_1} being true. \eqn{BF} is the Bayes Factor that in this case
#'  is the same as the LRT.
#'
#'  The SAM prior, denoted as \eqn{\pi_{sam}(\theta)}, is then defined
#'  as a mixture of an informative prior \eqn{\pi_1(\theta)}, constructed
#'  based on \eqn{D_h} and a non-informative prior \eqn{\pi_0(\theta)}:
#'
#'  \deqn{\pi_{sam}(\theta) = w\pi_1(\theta) + (1-w)\pi_0(\theta),}
#'  where the mixture weight \eqn{w} is calculated as:
#'
#'  \deqn{w = R / (1 + R).}
#'
#'  As the level of prior-data conflict increases, the likelihood ratio
#'  \eqn{R} decreases, resulting in a decrease in the weight \eqn{w}
#'  assigned to the informative prior and thus a decrease in information
#'  borrowing. As a result, \eqn{\pi_{sam}(\theta)} is data-driven and
#'  has the ability to self-adapt the information borrowing based on the
#'  degree of prior-data conflict.
#'
#' @return Displays the SAM prior as a mixture of an informative prior
#' (constructed based on the historical data) and a non-informative prior.
#'
#' @references Yang P, Zhao Y, Nie L, Vallejo J, Yuan Y.
#' SAM: Self-adapting mixture prior to dynamically borrow information from
#' historical data in clinical trials. \emph{Biometrics} 2023; 00, 1–12.
#' https://doi.org/10.1111/biom.13927
#' @references Schmidli H, Gsteiger S, Roychoudhury S, O'Hagan A,
#' Spiegelhalter D, Neuenschwander B.  Robust meta-analytic-predictive
#' priors in clinical trials with historical control information.
#' \emph{Biometrics} 2014; 70(4):1023-1032.
#'
#' @seealso \code{\link{SAM_weight}}
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' ## Examples for binary endpoints
#' ## Suppose that the informative prior constructed based on historical data is
#' ## beta(40, 60)
#' prior.historical <- RBesT::mixbeta(c(1, 40, 60))
#' ## Data of the control arm
#' data.control     <- stats::rbinom(60, size = 1, prob = 0.42)
#' ## Calculate the mixture weight of the SAM prior
#' wSAM <- SAM_weight(if.prior = prior.historical,
#'                    delta = 0.15,      ## Clinically significant difference
#'                    data = data.control  ## Control arm data
#'                    )
#' ## Assume beta(1,1) as the non-informative prior used for mixture
#' nf.prior  <- RBesT::mixbeta(nf.prior = c(1,1,1))
#' ## Generate the SAM prior
#' SAM.prior <- SAM_prior(if.prior = prior.historical, ## Informative prior
#'                        nf.prior = nf.prior,         ## Non-informative prior
#'                        weight = wSAM                ## Mixture weight of the SAM prior
#'                        )
#' graphics::plot(SAM.prior)
#'
#' ## Examples for continuous endpoints
#' ## Suppose that the informative prior constructed based on historical data is
#' ## N(0, 3)
#' sigma      <- 3
#' prior.mean <- 0
#' prior.se   <- sigma/sqrt(100)
#' prior.historical <- RBesT::mixnorm(c(1, prior.mean, prior.se), sigma = sigma)
#' ## Data of the control arm
#' data.control <- stats::rnorm(80, mean = 0, sd = sigma)
#' ## Calculate the mixture weight of the SAM prior
#' wSAM <- SAM_weight(if.prior = prior.historical,
#'                    delta = 0.2 * sigma,   ## Clinically significant difference
#'                    data = data.control     ## Control arm data
#'                    )
#' ## Assume unit-information prior N(0,3) as the non-informative prior used
#' ## for the mixture
#' nf.prior         <- RBesT::mixnorm(nf.prior = c(1,prior.mean, sigma),
#'                                    sigma = sigma)
#' ## Generate the SAM prior
#' SAM.prior <- SAM_prior(if.prior = prior.historical, ## Informative prior
#'                        nf.prior = nf.prior,         ## Non-informative prior
#'                        weight = wSAM                ## Mixture weight of the SAM prior
#'                        )
#' graphics::plot(SAM.prior)
#'
#'}
#' @import Metrics
#' @import RBesT
#' @import assertthat
#' @import checkmate
#' @import ggplot2
#' @import stats
#' @export
SAM_prior <- function(if.prior, nf.prior, weight, ...) UseMethod("SAM_prior")

#' @export
SAM_prior.default <- function(if.prior, nf.prior, weight, ...) "Unknown density"

#' @describeIn SAM_prior The function calculates the SAM prior for beta
#' mixture distribution. The default \code{nf.prior} is set to be
#' \code{mixbeta(c(1,1,1))} which represents a uniform prior \code{Beta(1,1)}.
#' @export
SAM_prior.betaMix <- function(if.prior, nf.prior, weight, ...) {
  checkmate::assert_number(weight, lower=0, upper=1)
  # checkmate::assert_number(n, lower=0, finite=TRUE)
  if(missing(nf.prior)) {
    message("Using default uniform prior as non-informative prior.")
    nf.prior <- RBesT::mixbeta(nf.prior = c(1,1,1))
  }
  # checkmate::assert_number(mean, lower=0, upper=1)
  # rob <- mixbeta(robust=c(1, mean, n+1), param="mn")
  # mixcombine(if.prior, rob, weight=c(weight, 1-weight))
  mixcombine(if.prior, nf.prior, weight=c(weight, 1-weight))

}

#' @describeIn SAM_prior The function calculates the SAM prior for gamma
#' mixture distribution. The default \code{nf.prior} is set to be
#' \code{mixgamma(c(1,0.001,0.001))} which represents a vague gamma prior
#' \code{Gamma(0.001,0.001)}.
#' @export
SAM_prior.gammaMix <- function(if.prior, nf.prior, weight, ...) {
  checkmate::assert_number(weight, lower=0, upper=1)
  # checkmate::assert_number(n, lower=0, finite=TRUE)
  if(missing(nf.prior)) {
    message(paste("Using default flat gamma prior as non-informative prior"))
    nf.prior <- mixgamma(nf.prior = c(1,0.001,0.001),
                         param = 'ab', likelihood = 'exp')
  }
  # checkmate::assert_number(mean, lower=0, finite=TRUE)
  # rob <- mixgamma(robust=c(1, mean, n), param="mn", likelihood=likelihood(if.prior))
  # mixcombine(if.prior, rob, weight=c(weight, 1-weight))
  mixcombine(if.prior, nf.prior, weight=c(weight, 1-weight))
}

#' @describeIn SAM_prior The function calculates the SAM prior for normal
#' mixture distribution. The default \code{nf.prior} is set to be
#' \code{mixnorm(c(1,summary(if.prior)['mean'], sigma))} which represents a
#' unit-information prior.
#' @param sigma Variance used for constructing the non-informative prior for
#' continuous endpoints.
#' @export
SAM_prior.normMix <- function(if.prior, nf.prior, weight, ..., sigma) {
  checkmate::assert_number(weight, lower=0, upper=1)
  # checkmate::assert_number(n, lower=0, finite=TRUE)
  # if(missing(mean)) {
  #   s <- summary(if.prior)
  #   message(paste("Using default mean for robust component; the mean of the prior which is", s["mean"], "."))
  #   mean <- s["mean"]
  # }
  # checkmate::assert_number(mean, finite=TRUE)
  if(missing(sigma)) {
    message("Using default prior reference scale ", RBesT::sigma(if.prior))
    sigma <- RBesT::sigma(if.prior)
  }
  if(missing(nf.prior)) {
    message(paste("Using default unit-information prior as non-informative prior"))
    nf.prior <- mixnorm(nf.prior = c(1,summary(if.prior)['mean'],sigma),
                        param = 'ms')
  }
  # rob <- mixnorm(robust=c(1, mean, n), param="mn", sigma=sigma)
  # mixcombine(if.prior, rob, weight=c(weight, 1-weight))
  mixcombine(if.prior, nf.prior, weight=c(weight, 1-weight))
}

