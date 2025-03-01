#' A function to fit Log-Normal renewal model
#' @param data as input inter-event times
#' @param t as user-specified time intervals (used to compute hazard rate)
#' @param y as user-specified time point (used to compute time-to-event probability)
#'
#' @return returns list of estimates after fitting Log-Normal renewal model
#' \describe{
#' \item{par1}{Estimated mean (on the log scale) of the Log-Normal model}
#' \item{par2}{Estimated standard deviation (on the log scale)of the Log-Normal model}
#' \item{logL}{Negative log-likelihood}
#' \item{AIC}{Akaike information criterion (AIC)}
#' \item{BIC}{Bayesian information criterion (BIC)}
#' \item{mu_hat}{Estimated mean}
#' \item{pr_hat}{Estimated (logit) probabilities}
#' \item{haz_hat}{Estimated (log) hazard rates}
#' }
#'
#' @examples
#' set.seed(42)
#' data <-  rgamma(100,3,0.01)
#'
#' # set some parameters
#' t = seq(100, 200, by=10)  # time intervals
#' y = 304  # cut-off year for estimating probablity
#'
#' # fit Log-Normal renewal model
#' result <- marp::lognorm_rp(data, t, y)
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

lognorm_rp <- function(data, t, y) {
  ## parameters
  par1 <- sum(log(data)) / length(data)
  par2 <- sqrt(sum((log(data) - par1) ^ 2) / length(data))
  ## log-likelihood, AIC and BIC
  logl <- sum(stats::dlnorm(data, par1, par2, log = TRUE))
  aic <- -2 * logl + 4
  bic <- -2 * logl + 2 * log(length(data))
  ## estimated mean, (logit) probability and (log) hazard rates
  mu_hat <- exp(par1 + par2 ^ 2 / 2)
  logitp <- gtools::logit(stats::plnorm(y, par1, par2))
  loghaz <- log(stats::dlnorm(t, par1, par2) / stats::plnorm(t, par1, par2, lower.tail = FALSE))
  return(list("par1" = par1,"par2" = par2,"logL" = logl,"AIC" = aic,"BIC" = bic,"mu_hat" = mu_hat,"pr_hat" = logitp,"haz_hat" = loghaz))
}
