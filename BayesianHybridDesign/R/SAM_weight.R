#' Calculating Mixture Weight of SAM Priors
#'
#' This function is exactly from the SAMprior R package version 1.1.1.
#' It is included to support the runSAM function in this package.
#'
#' The \code{SAM_weight} function is designed to calculate the mixture
#' weight of the SAM priors according to the degree of prior-data
#' conflicts (\emph{Yang, et al., 2023}).
#'
#' @param if.prior Informative prior constructed based on historical data,
#' represented (approximately) as a mixture of conjugate distributions.
#' @param theta.h Estimate of the treatment effect based on historical data.
#' If missing, the default value is set to be the posterior mean estimate from
#' \code{if.prior}.
#' @param method.w Methods used to determine the mixture weight for SAM priors.
#' The default method is "LRT" (Likelihood Ratio Test), the alternative option
#' is "PPR" (Posterior Probability Ratio). See Details section for more information.
#' @param prior.odds The prior probability of \eqn{H_0} being true compared to
#' the prior probability of \eqn{H_1} being true using PPR method. The default
#' value is 1. See Details section for more information.
#' @param data Data of the control arm from the current trial, see Methods
#' section for more details.
#' @param delta Clinically significant difference used for the SAM prior.
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
#' @return The mixture weight of the SAM priors.
#'
#' @references Yang P, Zhao Y, Nie L, Vallejo J, Yuan Y.
#' SAM: Self-adapting mixture prior to dynamically borrow information from
#' historical data in clinical trials. \emph{Biometrics} 2023; 00, 1–12.
#' https://doi.org/10.1111/biom.13927
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' ## Examples for binary endpoints
#' ## Example 1: no prior-data conflict
#' ## Suppose that the informative prior is beta(40, 60)
#' prior.historical <- RBesT::mixbeta(c(1, 40, 60))
#' ## Data of control arm
#' data.control     <- stats::rbinom(60, size = 1, prob = 0.42)
#' ## Calculate the mixture weight of the SAM prior
#' wSAM <- SAM_weight(if.prior = prior.historical,
#'                    delta = 0.15,      ## Clinically significant difference
#'                    data = data.control  ## Control arm data
#'                    )
#' print(wSAM)
#'
#' ## Example 2: with prior-data conflict (12 responses in 60 patients)
#' wSAM <- SAM_weight(if.prior = prior.historical,
#'                    delta = 0.15,
#'                    method.w = 'PPR',
#'                    prior.odds = 1/9,
#'                    n = 60,
#'                    r = 12
#'                    )
#' print(wSAM)
#'
#' ## Examples for continuous endpoints
#' ## Example 1: no prior-data conflict
#' ## Suppose the informative prior is N(0, 3)
#' sigma      <- 3
#' prior.mean <- 0
#' prior.se   <- sigma/sqrt(100)
#' prior.historical <- RBesT::mixnorm(c(1, prior.mean, prior.se), sigma = sigma)
#' ## Data of the control arm
#' data.control <- stats::rnorm(80, mean = 0, sd = sigma)
#' wSAM <- SAM_weight(if.prior = prior.historical,
#'                    delta = 0.3 * sigma,
#'                    data = data.control
#'                    )
#' print(wSAM)
#'}
#'
#' @import Metrics
#' @import RBesT
#' @import assertthat
#' @import checkmate
#' @import ggplot2
#' @import stats
#' @export
SAM_weight <- function(if.prior, theta.h, method.w, prior.odds, data, delta, ...) UseMethod("SAM_weight")

#' @export
SAM_weight.default <- function(if.prior, theta.h, method.w, prior.odds, data, delta, ...) "Unknown distribution"

#' @describeIn SAM_weight The function calculates the mixture weight of SAM
#' priors for beta mixture distribution. The input \code{data} can be patient-level
#' data (i.e., a vector of 0 and 1 representing the response status of each
#' patient) or summary statistics (i.e., the number of patients and the number
#' of responses).
#' @param n Number of subjects in the control arm for binary endpoint.
#' @param r Number of responses in the control arm for binary endpoint.
#' @export
SAM_weight.betaMix <- function(if.prior, theta.h, method.w, prior.odds, data, delta, n, r, ...) {
  if(!missing(data)) {
    assertthat::assert_that(all(data %in% c(0,1)))
    r <- sum(data)
    n <- length(data)
  }
  if(!missing(method.w)){
    assertthat::assert_that(all(method.w %in% c('LRT', 'PPR')))
    assertthat::assert_that(length(method.w) == 1)
  }

  if(missing(theta.h)){
    message("Using the posterior mean from informative prior as the estimate of the treatment effect based on historical data.")
    theta.h <- summary(if.prior)['mean']
  }

  if(missing(method.w)){
    message("Using the LRT (Likelihood Ratio Test) as the default method to calculate mixture weight for SAM priors.")
    method.w = 'LRT'
  }

  if(method.w == 'PPR' & missing(prior.odds)){
    message("Missing the prior odds, set as 1.")
    prior.odds = 1
  }

  ## The posterior mean estimation from the MAP prior
  theta_h_hat <- theta.h

  ## Calculate the weight for SAM prior
  R <- max(dbinom(x = r, size = n, prob = min(theta_h_hat + delta, 0.99)),
           dbinom(x = r, size = n, prob = max(theta_h_hat - delta, 0.01)))

  R <- R / dbinom(x = r, size = n, prob = theta_h_hat)

  if(method.w  == 'PPR') R = R / prior.odds

  ## SAM weight
  w = 1 / (1 + R)

  return(w)
}


#' @describeIn SAM_weight The function calculates the mixture weight of SAM
#' priors for normal mixture distribution. The input \code{data} should be
#' a vector of patient-level observations. The input \code{data} can be
#' patient-level data (i.e., a vector of continuous response of each
#' patient) or summary statistics (i.e., the mean estimate, number of subjects,
#' and the standard deviation in the control arm).
#' @param m Mean estimate in the control arm for continuous endpoint.
#' @param n Number of subjects in the control arm for continuous endpoint.
#' @param sigma Standard deviation in the control arm for continuous endpoint.
#' @export
SAM_weight.normMix <- function(if.prior, theta.h, method.w, prior.odds, data, delta, m, n, sigma, ...) {

  if(!missing(data)) {
    m <- mean(data)
    n <- length(data)
    if(n == 1 & missing(sigma)){
      stop("Standard deviation in the control arm must be given.")
    }else if(n > 1 & missing(sigma)){
        sigma <- sd(data)
    }
    ## Try to see if the summary data is provided
    if_summary <- FALSE
  } else {
    # if(missing(data)){
    #   stop("Individual data must be given.")
    # }
    if_summary = TRUE
  }

  if(!missing(method.w)){
    assertthat::assert_that(all(method.w %in% c('LRT', 'PPR')))
    assertthat::assert_that(length(method.w) == 1)
  }

  if(missing(theta.h)){
    message("Using the posterior mean from informative prior as the estimate of the treatment effect based on historical data.")
    theta.h <- summary(if.prior)['mean']
  }

  if(missing(method.w)){
    message("Using the LRT (Likelihood Ratio Test) as the default method to calculate mixture weight for SAM priors.")
    method.w = 'LRT'
  }

  if(method.w == 'PPR' & missing(prior.odds)){
    message("Missing the prior odds, set as 1.")
    prior.odds = 1
  }

  ## The posterior mean estimation from the MAP prior
  theta_h_hat <- theta.h

  ## Calculate the weight for SAM prior
  if(!if_summary){
    ## Based on individual data
    R1 <- sum(dnorm(data, mean = theta_h_hat - delta, sigma, log = T) - dnorm(data, mean = theta_h_hat, sigma, log = T))
    R2 <- sum(dnorm(data, mean = theta_h_hat + delta, sigma, log = T) - dnorm(data, mean = theta_h_hat, sigma, log = T))
    R  <- exp(max(R1, R2))
  }else{
    ## Based on summary data
    R1 <- -1/2 * ( (n*delta * (delta - 2 * m + 2 * theta_h_hat) ) / sigma^2 )
    R2 <- -1/2 * ( (n*delta * (delta + 2 * m - 2 * theta_h_hat) ) / sigma^2 )
    R  <- 1/exp(-max(R1, R2))
  }

  if(method.w  == 'PPR') R = R / prior.odds

  ## SAM weight
  w = 1 / (1 + R)


  return(w)
}


#' @describeIn SAM_weight The function calculates the mixture weight of SAM
#' priors for gamma mixture distribution. The input \code{data} can be
#' patient-level data (i.e., a matrix with the first row as the censoring
#' indicator and the second row recording the observed time) or summary
#' statistics (i.e., the number of uncensored observations \code{u} and
#' total observed time \code{w}).
#' @param u Number of events in the control arm for time-to-event endpoint.
#' @param w Total observed time in the control arm for time-to-event endpoint.
#' @export
SAM_weight.gammaMix <- function(if.prior, theta.h, method.w, prior.odds, data, delta, u, w, ...) {
  if(!missing(data)) {
    u <- sum(data[1,])
    w <- sum(data[2,])
  } else {
    if(missing(u) | missing(w)){
      stop("Individual or summary data must be given.")
    }
  }

  if(!missing(method.w)){
    assertthat::assert_that(all(method.w %in% c('LRT', 'PPR')))
    assertthat::assert_that(length(method.w) == 1)
  }

  if(missing(theta.h)){
    message("Using the posterior mean from informative prior as the estimate of the treatment effect based on historical data.")
    theta.h <- summary(if.prior)['mean']
  }

  if(missing(method.w)){
    message("Using the LRT (Likelihood Ratio Test) as the default method to calculate mixture weight for SAM priors.")
    method.w = 'LRT'
  }

  if(method.w == 'PPR' & missing(prior.odds)){
    message("Missing the prior odds, set as 1.")
    prior.odds = 1
  }

  ## The posterior mean estimation from the MAP prior
  hazard_h_hat <- theta.h

  ## The posterior mean estimation from the MAP prior
  # hazard_h_hat <- summary(if.prior)[theta.h]

  ## Calculate the weight for SAM prior
  R <- max(dgamma(hazard_h_hat + delta, shape = u + 1, rate = w),
           dgamma(hazard_h_hat - delta, shape = u + 1, rate = w))

  R <- R / dgamma(hazard_h_hat, shape = u + 1, rate = w)

  if(method.w  == 'PPR') R = R / prior.odds

  ## SAM weight
  w = 1 / (1 + R)


  return(w)
}

