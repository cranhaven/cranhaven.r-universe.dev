#' Generate survival times
#'
#'
#' @usage gen_surv_times(invchaz, mu = log(1), data, coxphmod = NULL)
#'
#' @param invchaz the inverse cumulative (baseline) hazard rate to be used
#' for generating survival times. Must take vector inputs!
#' @param mu the true hazard ratio used to generate survival times.
#' @param data an integer number of survival times to generate or
#'  (in combination with coxphmod): a \code{data.frame} containing
#'  subject covariates in named columns.
#' @param coxphmod (optional) a cox proportional hazards regression model as produced by
#' the function \code{\link[survival:coxph]{coxph()}}. Standard practice: \cr
#' \code{coxph(Surv(survtime, censorid) ~ covariates, data = data)}. \cr
#' Alternatively, a list with:
#' \itemize{
#' \item $formula (~ covariates)
#' \item $coefficients (named vector specifying risk adjustment coefficients
#' for covariates - names must be the same as in $formula and colnames of \code{data}).
#' }
#'
#' @details Sometimes it is desirable to generate survival times from an
#' increased hazard rate \deqn{h(t, \mu) = h_0(t) e^\mu}{h(t, \mu) = h_0(t) exp(\mu)}
#' with \eqn{h_0}{h_0} the baseline hazard rate. We call \eqn{e^\mu}{exp(\mu)} the true hazard ratio.
#'
#' @description Generate survival times according to hazard rate
#' \eqn{h(t) \exp(\mu)} with \eqn{h(t)} the hazard rate associated with the
#' specified inverse cumulative hazard rate \code{invchaz} and \eqn{\mu} the
#' specified true hazard ratio \code{mu}. See Bender et al. (2005).
#'
#'
#' @return A vector of survival times from subject entry time.
#'
#' @importFrom stats runif
#' @export
#'
#' @references Bender, R., Augustin, T., & Blettner, M. (2005).
#' Generating survival times to simulate Cox proportional hazards models.
#' Statistics in medicine, 24(11), 1713-1723. \doi{10.1002/sim.2059}
#'
#' @author Daniel Gomon
#' @family utils
#' @examples
#' gen_surv_times(invchaz = function(t) inv_chaz_exp(t, lambda = 0.01), data = 5)


gen_surv_times <- function(invchaz, mu = log(1), data, coxphmod = NULL){
  if(is.numeric(data) & length(data) == 1){
    U <- runif(data)
    Times <- invchaz(-log(U) * exp(-mu))
  } else if(is.data.frame(data)){
    n <- nrow(data)
    U <- runif(n)
    Times <- invchaz(-log(U) * exp(-mu)* (1/calc_risk(data, coxphmod)))
  } else(stop("Please provide either a number or data frame as data argument."))
  #calculate survival times using risk-adjusted inverse cdf generation
  return(Times)
}
