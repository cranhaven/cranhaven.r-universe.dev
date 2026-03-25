#' The predictive probability criterion function for Phase II single-arm design
#'
#' Lee and Liu's criterion function for determining the
#' trial decision cutoffs based on the predictive probability.
#'
#' @usage
#' PredP(x, n, nmax, a, b, p0, theta_t)
#' @param x the number of responses among \eqn{n} patients treated by the experimental drug at a certain stage of the trial.
#' @param n the number of patients treated by the experimental drug at a certain stage of the trial.
#' @param nmax the maximum number of patients treated by the experimental drug.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param p0 the the response rate for the standard drug.
#' @param theta_t the cutoff probability for efficacy including future patients; typically, \eqn{\theta_T = [0.85, 0.95]}. Set 0.9 by default.
#' @return
#' \item{prob}{the predictive probability: \eqn{PP = \sum\limits_{y=0}^{n_{max}-n} Pr(Y=y | x) I(\Pr(p > p_0 | Y=y, x) \geq \theta_T) }}
#' @references
#' Lee, J. J., Liu, D. D. (2008).
#' A predictive probability design for phase II cancer clinical trials.
#' \emph{Clinical Trials} \strong{5}: 93-106.
#'
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' # Using vague prior Uniform(0,1), i.e. Beta(1,1)
#' PredP(16, 23, 40, 1, 1, 0.5, 0.9)
#' @export

PredP <- function(x, n, nmax, a, b, p0, theta_t) {

  return(PredPCpp(x, n, nmax, a, b, p0, theta_t))

}


#' The stopping boundaries based on the predictive probability criterion
#'
#' The design function to sequentially monitor sample size and boundary based on Lee and Liu's criterion.
#'
#' @usage
#' PredP.design(type, nmax, a, b, p0, theta_t, theta, optimize)
#' @param type type of boundaries: "efficacy" or "futility".
#' @param nmax the maximum number of patients treated by the experimental drug.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param p0 the the response rate for the standard drug.
#' @param theta the cutoff probability: typically, \eqn{\theta = [0.9, 0.99]} for efficacy, \eqn{\theta = [0.01, 0.1]} for futility.
#' @param theta_t the cutoff probability for efficacy including future patients; typically, \eqn{\theta_T = [0.85, 0.95]}. Set 0.9 by default.
#' @param optimize logical value, if optimize=TRUE, then only output the minimal sample size for the same number of futility and efficacy boundaries.
#' @return
#' \item{boundset}{the boundaries set: \eqn{U_n} or \eqn{L_n}}
#' @references
#' Lee, J. J., Liu, D. D. (2008).
#' A predictive probability design for phase II cancer clinical trials.
#' \emph{Clinical Trials} \strong{5}: 93-106.
#'
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' PredP.design(type = "futility", nmax=40, a=1, b=1, p0=0.3, theta=0.05)
#' PredP.design(type = "efficacy", nmax=40, a=1, b=1, p0=0.3, theta=0.9)
#' @export
PredP.design <- function(type = c("efficacy", "futility"), nmax, a, b, p0, theta_t=0.9, theta, optimize=FALSE) {
    type <- match.arg(type)
    bound <- rep(NA, nmax)
    for (n in 1:nmax) {
      if (type == "efficacy") {
        for (x in 0:n) {
          if (PredP(x, n, nmax, a, b, p0, theta_t) >= theta) {
            bound[n] <- x
            break
          }
        }
      }
      else {
        for (x in n:0) {
          if (PredP(x, n, nmax, a, b, p0, theta_t) <= theta) {
            bound[n] <- x
            break
          }
        }
      }
    }
    boundset <- data.frame(n = 1:nmax, bound = bound)
  if (optimize==TRUE){
    return(boundset[!duplicated(boundset[, 2]), ])
  } else {return(boundset)}
}

