#' The posterior probability criterion function for Phase II single-arm design
#'
#' Thall and Simon's criterion function for determining the trial decision boundaries based on the posterior probability.
#'
#' @usage
#' PostP(x, n, a, b, p0)
#' @param x the number of responses among \eqn{n} patients treated by the experimental drug.
#' @param n the number of patients treated by the experimental drug.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param p0 the prespecified reseponse rate.
#' @return
#' \item{prob}{the posterior probability: \eqn{Pr(p > p_0  | X=x)}}
#' @references
#' Berry, S. M., Carlin, B. P., Lee, J. J., & Muller, P. (2010).
#' \emph{Bayesian adaptive methods for clinical trials.}
#' CRC press.
#'
#' Thall, P. F., Simon, R. (1994).
#' Practical Bayesian guidelines for phase IIB clinical trials.
#' \emph{Biometrics} \strong{50}: 337-349.
#'
#' Yin, G. (2013).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' PostP(8,15,1,1,0.8)
#' @importFrom stats pbeta
#' @export
PostP <- function(x, n, a, b, p0) {
  pbeta(p0, a+x, b+n-x, lower.tail = F)
}


#' The stopping boundaries based on the posterior probability criterion
#'
#' The design function to sequentially monitor sample size and boundary based on Thall and Simon's criterion.
#'
#' @usage
#' PostP.design(type, nmax, a, b, p0, theta, optimize)
#' @param type type of boundaries: "efficacy" or "futility".
#' @param nmax the maximum number of patients treated by the experimental drug.
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param p0 the pre-specified reseponse rate.
#' @param theta the cutoff probability: typically, \eqn{\theta = [0.9, 0.99]} for efficacy, \eqn{\theta = [0.01, 0.1]} for futility.
#' @param optimize logical value, if optimize=TRUE, then only output the minimal sample size for the same number of futility and efficacy boundaries.
#' @return
#' \item{boundset}{the boundaries set: \eqn{U_n} or \eqn{L_n}}
#' @references
#' Thall, P. F., Simon, R. (1994).
#' Practical Bayesian guidelines for phase IIB clinical trials.
#' \emph{Biometrics} \strong{50}: 337-349.
#'
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' ## Using vague prior Unif(0,1)
#' PostP.design(type = "futility", nmax=100, a=1, b=1, p0=0.3, theta=0.05)
#' PostP.design(type = "efficacy", nmax=100, a=1, b=1, p0=0.3, theta=0.9)
#' ## Or using Jeffery prior with Beta(0.5,0.5)
#' PostP.design(type = "futility", nmax=100, a=0.5, b=0.5, p0=0.3, theta=0.05)
#' PostP.design(type = "efficacy", nmax=100, a=0.5, b=0.5, p0=0.3, theta=0.9)
#' @export
PostP.design <- function(type = c("efficacy", "futility"), nmax, a, b, p0, theta, optimize=FALSE) {
  type <- match.arg(type)
  bound <- rep(NA, nmax)
  for (n in 1:nmax) {
    if (type == "efficacy") {
      for (x in 0:n) {
        if (PostP(x, n, a, b, p0) >= theta) {
          bound[n] <- x
          break
        }
      }
    } else {
      for (x in n:0) {
        if (PostP(x, n, a, b, p0) <= theta) {
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

