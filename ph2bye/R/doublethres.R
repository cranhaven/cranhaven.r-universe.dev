#' The whole design with double thresholds showing futility and efficacy boundary together
#'
#' The design function to sequentially monitor sample size and stopping boundary for both futility and efficacy
#'
#' @usage
#' DT.design(type, a, b, nmin, nmax, p0, p1, theta0, theta1, theta_t, optimize)
#'
#' @param type type of stopping criterion: "PostP" or "PredP".
#' @param a the hyperparameter (shape1) of the Beta prior for the experimental drug.
#' @param b the hyperparameter (shape2) of the Beta prior for the experimental drug.
#' @param nmin the minimum number of patients treated by the experimental drug.
#' @param nmax the maximum number of patients treated by the experimental drug.
#' @param p0 the pre-specified reseponse rate.
#' @param p1 the pre-specified reseponse rate.
#' @param theta0 the cutoff probability for futility: typically, \eqn{\theta_0 = [0.01, 0.1]}.
#' @param theta1 the cutoff probability for efficacy: typically, \eqn{\theta_1 = [0.9, 0.99]}.
#' @param theta_t the cutoff probability for efficacy including future patients; typically, \eqn{\theta_T = [0.85, 0.95]}. Set 0.9 by default.
#' @param optimize logical value, if optimize=TRUE, then only output the minimal sample size for the same number of futility and efficacy boundaries.
#' @return
#' \item{boundsets}{the boundaries sets: \eqn{U_n} and \eqn{L_n}}
#' @references
#' Thall, P. F., Simon, R. (1994).
#' Practical Bayesian guidelines for phase IIB clinical trials.
#' \emph{Biometrics} \strong{50}: 337-349.
#'
#' Lee, J. J., Liu, D. D. (2008).
#' A predictive probability design for phase II cancer clinical trials.
#' \emph{Clinical Trials} \strong{5}: 93-106.
#'
#' Yin, G. (2012).
#' \emph{Clinical Trial Design: Bayesian and Frequentist Adaptive Methods.}
#' New York: Wiley.
#' @examples
#' ## Using vague prior Unif(0,1), sequential monitor
#' DT.design(type = "PostP", a=1, b=1, nmin=20, nmax=60, p0=0.4, p1=0.3, theta0 = 0.05, theta1 = 0.9)
#' DT.design(type = "PredP", a=1, b=1, nmin=20, nmax=60, p0=0.4, p1=0.3, theta0 = 0.05, theta1 = 0.9,
#' theta_t = 0.9)
#' ## Or using Jeffery prior with Beta(0.5,0.5), multi-stage monitor when sample size is
#' ## 10, 20, ..., 80
#' DT.design(type = "PostP", a=0.5, b=0.5, nmin=1, nmax=85, p0=0.3, p1=0.3, theta0 = 0.05,
#' theta1 = 0.9)[(1:8)*10,]
#' DT.design(type = "PredP", a=0.5, b=0.5, nmin=1, nmax=85, p0=0.3, p1=0.3, theta0 = 0.05,
#' theta1 = 0.9, theta_t = 0.9)[(1:8)*10,]
#' @export

DT.design <- function(type=c("PostP","PredP"), a, b, nmin, nmax, p0,p1, theta0, theta1, theta_t=0.9, optimize=FALSE){
  type <- match.arg(type)
  if (theta0>theta1) stop("The efficacy threshold should be greater than futility threshold.")
  res <- switch(type,
                PostP = {
                  merge(PostP.design(type = "futility", a = a, b =b, nmax = nmax, p0 = p0, theta = theta0, optimize=optimize),
                        PostP.design(type = "efficacy", a = a, b = b, nmax = nmax, p0 = p1, theta = theta1, optimize=optimize),
                        by="n")},
                PredP = {
                  merge(PredP.design(type = "futility", a = a, b = b, nmax = nmax, p0 = p0, theta = theta0, theta_t = theta_t, optimize=optimize),
                        PredP.design(type = "efficacy", a = a, b = b, nmax = nmax, p0 = p1, theta = theta1, theta_t = theta_t, optimize=optimize),
                        by="n")})
  names(res) <- c("N", "Futility", "Efficacy")
  return(res[which(res$`N`>=nmin),])
}
