#' A function to apply model-averaged renewal process
#' @param data input inter-event times
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param m the number of iterations in nlm
#' @param y user-specified time point (used to compute time-to-event probability)
#' @param which.model user-specified generating (or true underlying if known) model
#'
#' @return returns list of estimates obtained from different renewal processes and after applying model-averaging
#' \describe{
#' \item{par1}{Estimated scale parameters (if applicable) of all six renewal models}
#' \item{par2}{Estimated shape parameters (if applicable) of all six renewal models}
#' \item{logL}{Negative log-likelihood}
#' \item{AIC}{Akaike information criterion (AIC)}
#' \item{BIC}{Bayesian information criterion (BIC)}
#' \item{mu_hat}{Estimated mean}
#' \item{pr_hat}{Estimated (logit) probabilities}
#' \item{haz_hat}{Estimated (log) hazard rates}
#' \item{weights_AIC}{Model weights calculated based on AIC}
#' \item{weights_BIC}{Model weights calculated based on BIC}
#' \item{model_best}{Model selected based on the lowest AIC}
#' \item{mu_best}{Estimated mean obtained from the model with the lowest AIC}
#' \item{pr_best}{Estimated probability obtained from the model with the lowest AIC}
#' \item{haz_best}{Estimated hazard rates obtained from the model with the lowest AIC}
#' \item{mu_gen}{Estimated mean obtained from the (true or hypothetical) generating model }
#' \item{pr_gen}{Estimated probability obtained from the (true or hypothetical) generating model }
#' \item{haz_gen}{Estimated hazard rates obtained from the (true or hypothetical) generating model }
#' \item{mu_aic}{Estimated mean obtained from model-averaging (using AIC weights)}
#' \item{pr_aic}{Estimated probability obtained from model-averaging (using AIC weights)  }
#' \item{haz_aic}{Estimated hazard rates obtained from model-averaging (using AIC weights)}
#' }
#'
#' @examples
#' set.seed(42)
#' data <-  rgamma(100,3,0.01)
#'
#' # set some parameters
#' m = 10  # number of iterations for MLE optimization
#' t = seq(100, 200, by=10)  # time intervals
#' y = 304  # cut-off year for estimating probability
#' which.model <- 2 # specify the generating model
#'
#' # model selection and averaging
#' result <- marp::marp(data, t, m, y, which.model)
#'
#' @export

### Model-Averaged Renewal Processes -------------------------------
marp <- function(data,t,m,y,which.model=1) {
  out <- mapply(c,
                poisson_rp(data, t, y), ## 1. Poisson renewal model
                gamma_rp(data, t, m, y), ## 2. Gamma renewal model
                loglogis_rp(data, t, m, y), ## 3. Log-Logistics renewal model
                weibull_rp(data, t, m, y), ## 4. Weibull renewal model
                lognorm_rp(data, t, y), ## 5. Log-Normal renewal model
                bpt_rp(data, t, m, y), ## 6. BPT renewal model
                USE.NAMES = TRUE, SIMPLIFY = FALSE)
  ## estimated mean, (logit) probability and (log) hazard rates from six renewal models
  mu_hat <- out$mu_hat
  pr_hat <- out$pr_hat
  haz_hat <- matrix(out$haz_hat, length(t), 6)
  ## find the best model (minimum AIC value)
  which_aic <- which.min(out$AIC)
  ## min. AIC
  min_aic <- out$AIC[which_aic]
  ## delta AIC for each model (diff bewteen AIC of each model and the min. AIC)
  delta_aic <- out$AIC - min_aic
  ## compute AIC weights
  aic_weight <- exp(-.5 * delta_aic) / sum(exp(-.5 * delta_aic))
  # which_bic <- which.min(out$BIC)
  # min_bic <-  out$BIC[which_bic]
  # delta_bic <- out$BIC - min_bic
  # bic_weight <- exp(-.5 * delta_bic) / sum(exp(-.5 * delta_bic))
  ## model-averaged mean, (logit) probability and (log) hazard rates
  ## using AIC weights
  mu_aic <- mu_hat %*% aic_weight
  pr_aic <- pr_hat %*% aic_weight
  haz_aic <- haz_hat %*% aic_weight
  out1 <-list("weights_AIC" = aic_weight,
              # "weights_BIC" = bic_weight,
              "model_best" = which_aic,
              ## best model is defined as the model with the lowest AIC
              "mu_best" = mu_hat[which_aic],
              "pr_best" = pr_hat[which_aic],
              "haz_best" = haz_hat[, which_aic],
              ## default generating model is a Poisson renewal model
              "mu_gen" = mu_hat[which.model],
              "pr_gen" = pr_hat[which.model],
              "haz_gen" = haz_hat[, which.model],
              ## model-averaging using AIC weights
              "mu_aic" = mu_aic,
              "pr_aic" = pr_aic,
              "haz_aic" = as.numeric(haz_aic))
  return(append(out, out1))
}
