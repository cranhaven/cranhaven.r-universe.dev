#' A function to calculate percentile bootstrap confidence interval
#' @param data input inter-event times
#' @param m the number of iterations in nlm
#' @param B number of bootstrap samples
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param y user-specified time point (used to compute time-to-event probability)
#' @param which.model user-specified generating (or true underlying if known) model
#'
#' @return returns list of percentile bootstrap intervals (including the model-averaged approach).
#' \describe{
#' \item{weights_bstp}{Model weights calculated by bootstrapping, that is, the frequency of each model being selected as the best model is divided by the total number of bootstraps}
#' \item{mu_gen}{Median of the percentile bootstrap confidence interval of the estimated mean based on the generating model}
#' \item{mu_gen_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated mean based on the generating model}
#' \item{mu_gen_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated mean based on the generating model}
#' \item{mu_best}{Median of the percentile bootstrap confidence interval of the estimated mean based on the best model}
#' \item{mu_best_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated mean based on the best model}
#' \item{mu_best_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated mean based on the best model}
#' \item{pr_gen}{Median of the percentile bootstrap confidence interval of the estimated probabilities  based on the generating model}
#' \item{pr_gen_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated probabilities  based on the generating model}
#' \item{pr_gen_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated probabilities  based on the generating model}
#' \item{pr_best}{Median of the percentile bootstrap confidence interval of the estimated probabilities  based on the best model}
#' \item{pr_best_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated probabilities  based on the best model}
#' \item{pr_best_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated probabilities  based on the best model}
#' \item{haz_gen}{Median of the percentile bootstrap confidence interval of the estimated hazard rates  based on the generating model}
#' \item{haz_gen_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated hazard rates  based on the generating model}
#' \item{haz_gen_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated hazard rates  based on the generating model}
#' \item{haz_best}{Median of the percentile bootstrap confidence interval of the estimated hazard rates  based on the best model}
#' \item{haz_best_lower}{Lower limit of the percentile bootstrap confidence interval of the estimated hazard rates  based on the best model}
#' \item{haz_best_upper}{Upper limit of the percentile bootstrap confidence interval of the estimated hazard rates  based on the best model}
#' }
#'
#' @examples
#' \donttest{
#' # generate random data
#' set.seed(42)
#' data <- rgamma(30, 3, 0.01)
#'
#' # set some parameters
#' m <- 10 # number of iterations for MLE optimization
#' t <- seq(100,200,by=10) # time intervals
#' y <- 304 # cut-off year for estimating probablity
#' B <- 100 # number of bootstraps
#' BB <- 100 # number of double bootstraps
#' which.model <- 2 # specify the generating model
#'
#' # construct percentile bootstrap confidence invtervals
#' marp::percent_confint(data, B, t, m, y, which.model)
#' }
#'
#' @export

percent_confint <-  function(data, B, t, m, y, which.model = 1) {
  ## non-parametric bootstraps
  bstrp <- replicate(B, sample(data, size = length(data), replace = TRUE))
  ## fit six renwal models
  out <- apply(bstrp, 2, function(x) marp(x, t, m, y, which.model))
  ## locate the best model in each bootstrap
  model_best <- sapply(out, '[[', 10)
  ## compute bootstrap weights (propotion of each model is selected as the best across total bootstraps)
  weights_bstp <- sapply(1:6, function(i) length(which(i == model_best)) / B)
  ## using the best model of each bootstrap
  ## find estimated mean, (logit) probability and (log) hazard rates
  mu_best <- sapply(out, '[[', 11)
  pr_best <- sapply(out, '[[', 12)
  haz_best <- sapply(out, '[[', 13)
  ## find percentile bootstrap CIs of estimated mean, (logit) probability and (log) hazard rates
  mu_best_ci <- stats::quantile(mu_best, probs = c(0.025, 0.5, 0.975))
  pr_best_ci <- stats::quantile(pr_best, probs = c(0.025, 0.5, 0.975))
  haz_best_ci <- apply(haz_best, 1, function(x) stats::quantile(x, probs = c(0.025, 0.5, 0.975)))
  ## using the generating model of each bootstrap
  ## find estimated mean, (logit) probability and (log) hazard rates
  mu_gen <- sapply(out, '[[', 14)
  pr_gen <- sapply(out, '[[', 15)
  haz_gen <- sapply(out, '[[', 16)
  ## find percentile bootstrap CIs of estimated mean, (logit) probability and (log) hazard rates
  mu_gen_ci <-  stats::quantile(mu_gen, probs = c(0.025, 0.5, 0.975))
  pr_gen_ci <-  stats::quantile(pr_gen, probs = c(0.025, 0.5, 0.975))
  haz_gen_ci <- apply(haz_gen, 1, function(x) stats::quantile(x, probs = c(0.025, 0.5, 0.975)))
  return(list('weights_bstp' = weights_bstp,
              'mu_gen' = as.numeric(mu_gen_ci[2]),
              'mu_gen_lower' = as.numeric(mu_gen_ci[1]),
              'mu_gen_upper' = as.numeric(mu_gen_ci[3]),
              'mu_best' = as.numeric(mu_best_ci[2]),
              'mu_best_lower' = as.numeric(mu_best_ci[1]),
              'mu_best_upper' = as.numeric(mu_best_ci[3]),
              'pr_gen' = as.numeric(pr_gen_ci[2]),
              'pr_gen_lower' = as.numeric(pr_gen_ci[1]),
              'pr_gen_upper' = as.numeric(pr_gen_ci[3]),
              'pr_best' = as.numeric(pr_best_ci[2]),
              'pr_best_lower' = as.numeric(pr_best_ci[1]),
              'pr_best_upper' = as.numeric(pr_best_ci[3]),
              'haz_gen' = as.numeric(haz_gen_ci[2,]),
              'haz_gen_lower' = as.numeric(haz_gen_ci[1,]),
              'haz_gen_upper' = as.numeric(haz_gen_ci[3,]),
              'haz_best' = as.numeric(haz_best_ci[2,]),
              'haz_best_lower' = as.numeric(haz_best_ci[1,]),
              'haz_best_upper' = as.numeric(haz_best_ci[3,])))
}
