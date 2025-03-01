#' A function to fit Poisson renewal model
#' @param data input inter-event times
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param y user-specified time point (used to compute time-to-event probability)
#'
#' @return returns list of estimates after fitting Poisson renewal model
#' \describe{
#' \item{par1}{Estimated parameter of the Poisson model}
#' \item{par2}{N/A, only keep it as a place holder for output formatting purpose}
#' \item{logL}{Negative log-likelihood}
#' \item{AIC}{Akaike information criterion (AIC)}
#' \item{BIC}{Bayesian information criterion (BIC)}
#' \item{mu_hat}{Estimated mean}
#' \item{pr_hat}{Estimated (logit) probabilities}
#' \item{haz_hat}{Estimated (log) hazard rates}
#' }
#' @examples
#' set.seed(42)
#' data <-  rgamma(100,3,0.01)
#'
#' # set some parameters
#' t = seq(100, 200, by=10)  # time intervals
#' y = 304  # cut-off year for estimating probablity
#'
#' # fit Poisson renewal model
#' result <- marp::poisson_rp(data, t, y)
#'
#' # print result
#' cat("par1 = ", result$par1, "\n")
#' cat("par2 = ", result$par2, "\n")
#' cat("logL = ", result$logL, "\n")
#' cat("AIC = ", result$AIC, "\n")
#' cat("BIC = ", result$BIC, "\n")
#' cat("mu_hat = ", result$mu_hat, "\n")
#' cat("pr_hat = ", result$pr_hat, "\n")
#'
#' @export

poisson_rp <- function(data, t, y) {
  ## parameters (one-parameter model)
  par1 <- 1 / mean(data) # lambda of exponential distribution
  par2 <- NA
  ## log-likelihood, AIC and BIC
  logl <- sum(stats::dexp(data, par1, log = TRUE)) # log-likelihood
  aic <- -2 * logl + 2 # AIC
  bic <- -2 * logl + log(length(data)) # BIC
  ## estimated mean, (logit) probability and (log) hazard rates
  mu_hat <- 1 / par1
  logitp <- gtools::logit(stats::pexp(y, par1))
  loghaz <- log(rep(par1, length(t)))
  return(list("par1" = par1,"par2" = par2,"logL" = logl,"AIC" = aic,"BIC" = bic,"mu_hat" = mu_hat,"pr_hat" = logitp,"haz_hat" = loghaz))
}
