#' The posterior probability criterion function for Phase II single-arm design
#'
#' Thall, Simon and Estey's criterion function for determining the trial decision boundaries for efficacy (futility) and safety (toxicity).
#'
#' @usage
#' MultPostP(x, n, a.vec, p0)
#' @param x the value of observed data.  It can be \eqn{x_{E}=y_{ET}+y_{E T^C}} i.e. number of responses for efficacy among \eqn{n} patients treated by the experimental drug, or \eqn{x_{T}= y_{ET}+y_{E^C T}} i.e. number of responses for toxicity among \eqn{n} patients treated by the experimental drug, where \eqn{y = (y_{ET}, y_{E^C T}, y_{ET^C}, y_{E^C T^C})}, that is, among \eqn{n} patients treated by the experimental drug, \eqn{y_{ET}} of them have experienced both toxicity and efficacy, \eqn{y_{E^C T}} have experienced toxicity only, \eqn{y_{ET^C}} have experienced efficacy only, \eqn{y_{E^C T^C}} have neither experienced toxicity nor efficacy.
#' @param n the number of patients treated by the experimental drug at a certain stage of the trial.
#' @param a.vec the hyperparameter vector of the Dirichlet prior for the experimental drug.
#' @param p0 the prespecified reseponse rate for efficacy, futility or toxicity.
#' @return
#' \item{prob}{the posterior probability: \eqn{Pr(p_E > p_0 | X=x_E)} or \eqn{Pr(p_T > p_0 | X=x_T)}}
#' @references
#' Berry, S. M., Carlin, B. P., Lee, J. J., & Muller, P. (2010).
#' \emph{Bayesian adaptive methods for clinical trials.}
#' CRC press.
#'
#' Thall, Peter F., Richard M. Simon, and Elihu H. Estey. (1995).
#' \emph{Bayesian sequential monitoring designs for single-arm clinical trials with multiple outcomes.}
#' \emph{Statistics in medicine} \strong{14.4}: 357-379.
#'
#' Yin, G. (2013).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' n <- 30; x.eff <- 5; x.tox <- 8; param <- c(1,1,1,1); p0.eff <- 0.9; p0.tox <- 0.95
#' MultPostP(x=x.eff, n=n, a.vec=param, p0=p0.eff)
#' MultPostP(x=x.tox, n=n, a.vec=param, p0=p0.tox)
#' @importFrom stats pbeta
#' @export
MultPostP <- function(x, n, a.vec, p0) {
  p.e <- pbeta(p0, sum(a.vec[c(1,3)])+x, sum(a.vec[c(2,4)])+n-x, lower.tail = F)
  p.t <- pbeta(p0, sum(a.vec[c(1,2)])+x, sum(a.vec[c(3,4)])+n-x, lower.tail = F)
  return(c(p.e,p.t))
}




#' The stopping boundaries based on the multiple outcomes criterion
#'
#' The design function to sequentially monitor sample size and boundary based on Thall, Simon and Estey's criterion.
#'
#' @usage
#' MultPostP.design(type, nmax, a.vec, p0, theta, optimize)
#' @param type type of boundaries: "efficacy" or "futility" or "toxicity".
#' @param nmax the maximum number of patients treated by the experimental drug.
#' @param a.vec the hyperparameter vector of the Dirichlet prior for the experimental drug.
#' @param p0 the prespecified reseponse rate for efficacy or toxicity.
#' @param theta the cutoff probability: typically, \eqn{\theta = [0.9, 0.99]} for efficacy, \eqn{\theta = [0.01, 0.1]} for futility, and \eqn{\theta = [0.95, 1]} for toxicity.
#' @param optimize logical value, if optimize=TRUE, then only output the minimal sample size for the same number of futility boundaries and maximal sample size for the same number efficacy boundaries

#' @return
#' \item{boundset}{the boundaries set: \eqn{U_n} or \eqn{L_n} for the experimental drug efficacy or futility; \eqn{T_n} for the experimental drug toxicity.}
#' @references
#' Thall, Peter F., Richard M. Simon, and Elihu H. Estey. (1995).
#' \emph{Bayesian sequential monitoring designs for single-arm clinical trials with multiple outcomes.}
#' \emph{Statistics in medicine} \strong{14.4}: 357-379.
#'
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' ## Using vague prior Unif(0,1)
#' MultPostP.design(type="futility",nmax = 30,a.vec = c(1,1,1,1),p0 = 0.15, theta = 0.05)
#' MultPostP.design(type="efficacy",nmax = 30,a.vec = c(1,1,1,1),p0 = 0.15, theta = 0.9)
#' MultPostP.design(type="toxicity",nmax = 30,a.vec = c(1,1,1,1),p0 = 0.15, theta = 0.95)
#' @export
MultPostP.design <- function(type = c("efficacy", "futility", "toxicity"), nmax, a.vec, p0, theta, optimize=FALSE) {
  type <- match.arg(type)
  bound <- rep(NA, nmax)
  for (n in 1:nmax) {
    if (type == "efficacy") {
        for (x in 0:n) {
        if (MultPostP(x, n, a.vec, p0)[1] > theta) {
          bound[n] <- x
          break
        }
      }
    } else if (type=="futility") {
      for (x in n:0) {
        if (MultPostP(x, n, a.vec, p0)[1] < theta) {
          bound[n] <- x
          break
        }
      }
    } else {
      for (x in 0:n) {
        if (MultPostP(x, n, a.vec, p0)[2] > theta) {
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
