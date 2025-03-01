#' A function to fit Log-Logistics renewal model
#' @param data input inter-event times
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param m the number of iterations in nlm
#' @param y user-specified time point (used to compute time-to-event probability)
#'
#' @return returns list of estimates after fitting Log-Logistics renewal model
#'
#' \describe{
#' \item{par1}{Estimated shape parameter of the Log-Logistics model}
#' \item{par2}{Estimated scale parameter of the Log-Logistics model}
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
#' m = 10  # number of iterations for MLE optimization
#' t = seq(100, 200, by=10)  # time intervals
#' y = 304  # cut-off year for estimating probablity
#'
#' # fit Log-Logistic renewal model
#' result <- marp::loglogis_rp(data, t, m, y)
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

loglogis_rp <- function(data, t, m, y) {
  ## find MLE via numerical optimization (nlm)
  i <- 1
  inits <- NULL
  loop <- NULL
  data_log_median <- log(stats::median(data))
  data_log_mean <- log(mean(data))
  while (i < m) {
    tmp_init <-
      cbind(stats::runif(1, 0, 2 * data_log_median), stats::runif(1, 0, 2 * data_log_mean / data_log_median))
    tryCatch({
      tmp <- stats::nlm(loglogis_logl, log(tmp_init), x = data)
      if (tmp$code <= 2.5) {
        eval(parse(text = paste("tmp", i, '=tmp', sep = "")))
        loop <- c(loop, tmp$minimum)
        i <- i + 1
      }
    }, error = function(e) {
    })
  }
  index <- which.min(loop)
  mle <- get(paste("tmp", index, sep = ""))
  ## log-likelihood, AIC and BIC
  logl <- mle$minimum
  aic <- 2 * logl + 4
  bic <- 2 * logl + 2 * log(length(data))
  ## parameters
  par1 <- exp(mle$estimate[2])
  par2 <- exp(mle$estimate[1])

  if (par1 > 1) { ## mean is undefined if par1 (shape parameter) < 1
    mu_hat <- (par2 * pi / par1) / sin(pi / par1)
  } else{ ## set to be sample mean
    mu_hat <- mean(data)
  }
  logitp <- gtools::logit(pllog(y, par1, par2))
  loghaz <- log(dllog(t, par1, par2) / pllog(t, par1, par2, lower.tail = FALSE))
  return(list("par1" = par1,"par2" = par2,"logL" = -logl,"AIC" = aic,"BIC" = bic,"mu_hat" = mu_hat,"pr_hat" = logitp,"haz_hat" = loghaz))
}
