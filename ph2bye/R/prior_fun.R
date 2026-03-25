#' Beta priors determination based on different prior information
#'
#' The prior function to calculate the Beta prior parameters
#'
#' @usage
#' prior(type, mu, v, N, W, init)
#'
#' @param type type of prior information: "MeanVar" uses mean and variance, "Optimist" uses (ORR) mean, "ORRN" uses ORR and smaple size, "ORRW" uses ORR and CI width.
#' @param mu prior(ORR) mean.
#' @param v prior variance
#' @param N prior sample size.
#' @param W prior confidence interval width.
#' @param init initial value to solve the nonlinear equations for "ORRW" type.
#' @return
#' \item{parameters}{the vector of Beta parameters: \eqn{a} and \eqn{b}}
#' @references
#' Thall, P. F., Simon, R. (1994).
#' Practical Bayesian guidelines for phase IIB clinical trials.
#' \emph{Biometrics} \strong{50}: 337-349.
#'
#' Mayo, M. S., & Gajewski, B. J. (2004).
#' Bayesian sample size calculations in phase II clinical trials using informative conjugate priors.
#' \emph{Controlled clinical trials} \strong{25}(2): 157-167.
#'
#' Tan, S. B., & Machin, D. (2002).
#' Bayesian two-stage designs for phase II clinical trials.
#' \emph{Statistics in medicine} \strong{21}(14): 1991-2012.
#' New York: Wiley.
#' @examples
#' prior(type = "MeanVar", mu=0.2, v=0.025)
#' prior(type = "Optimist", mu = 0.2)
#' prior(type = "ORRN", mu = 0.2, N = 10)
#' prior(type = "ORRW", mu = 0.2, W = 0.5)
#' @importFrom stats pbeta qbeta
#' @importFrom nleqslv nleqslv
#' @export
prior <- function(type=c("MeanVar", "Optimist", "ORRN", "ORRW"), mu, v, N, W, init=c(0.5,0.5)){
  type <- match.arg(type)
  switch (type,
    MeanVar = {c(a = mu^2*(1-mu)/v - mu, b = mu*(1-mu)^2/v - (1-mu))},
    Optimist = {c(a = mu+1,b=2-mu)},
    ORRN = {c(a=mu+1+mu*N, b=(1-mu)+1+N*(1-mu))},
    ORRW = {
      sol <- function(x){
      y <- numeric(2)
      y[1] <- x[1]/(x[1]+x[2])-mu
      y[2] <- qbeta(p = 0.975, shape1 = x[1], shape2 = x[2])-qbeta(p = 0.025, shape1 = x[1], shape2 = x[2])-W
      y
    }
      res <- nleqslv(init, sol, control=list(trace=1,btol=.01,delta="newton"))$'x'
      c(a=res[1],b=res[2])
  })
}
