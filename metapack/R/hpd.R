#' get the highest posterior density (HPD) interval
#' @param object the output model from fitting a (network) meta analysis/regression model
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the probability which the HPD interval will cover
#' @param HPD a logical value indicating whether HPD or equal-tailed credible interval should be computed; by default, TRUE
#' @details A \eqn{100(1-\alpha)}% HPD interval for \eqn{\theta} is given by \deqn{R(\pi_\alpha) = {\theta: \pi(\theta| D) \ge \pi_\alpha},} where \eqn{\pi_\alpha} is the largest constant that satisfies \eqn{P(\theta \in  R(\pi_\alpha)) \ge 1-\alpha}. `hpd` computes the HPD interval from an MCMC sample by letting \eqn{\theta_{(j)}} be the \eqn{j}th smallest of the MCMC sample, \eqn{{\theta_i}} and denoting \deqn{R_j(n) = (\theta_{(j)}, \theta_{(j+[(1-\alpha)n])}),} for \eqn{j=1,2,\ldots,n-[(1-\alpha)n]}. Once \eqn{\theta_i}'s are sorted, the appropriate \eqn{j} is chosen so that \deqn{\theta_{(j+[(1-\alpha)n])} - \theta_{(j)} = \min_{1\le j \leq n-[(1-\alpha)n]} (\theta_{(j+[(1-\alpha)n])} - \theta_{(j)}).}
#' @references
#' Chen, M. H., & Shao, Q. M. (1999). Monte Carlo estimation of Bayesian credible and HPD intervals. *Journal of Computational and Graphical Statistics*, **8(1)**, 69-92.
#' @md
#' @return dataframe containing HPD intervals for the parameters
#' @export
"hpd" <- function(object, parm, level = 0.95, HPD = TRUE) {
    UseMethod("hpd", object)
} 
