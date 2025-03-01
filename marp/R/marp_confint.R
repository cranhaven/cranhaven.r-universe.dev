#' A function to apply model-averaged renewal process
#' @param data input inter-event times
#' @param m the number of iterations in nlm
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param B number of bootstrap samples
#' @param BB number of double-bootstrap samples
#' @param alpha significance level
#' @param y user-specified time point (used to compute time-to-event probability)
#' @param which.model user-specified generating (or true underlying if known) model
#'
#' @return returns list of point and interval estimation obtained from different renewal models (including model-averaged confidence intervals).
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
#' \item{mu_bstrp}{Estimated mean obtained from model-averaging (using bootstrapped weights)}
#' \item{pr_bstrp}{Estimated probability obtained from model-averaging (using bootstrapped weights)  }
#' \item{haz_bstrp}{Estimated hazard rates obtained from model-averaging (using bootstrapped weights)}
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
#' \item{mu_lower_gen}{Lower limit of the studentized bootstrap confidence interval of the estimated mean based on the generating model}
#' \item{mu_upper_gen}{Upper limit of the studentized bootstrap confidence interval of the estimated mean based on the generating model}
#' \item{mu_lower_best}{Lower limit of the studentized bootstrap confidence interval of the estimated mean based on the best model}
#' \item{mu_upper_best}{Upper limit of the studentized bootstrap confidence interval of the estimated mean based on the best model}
#' \item{pr_lower_gen}{Lower limit of the studentized bootstrap confidence interval of the estimated probabilities  based on the generating model}
#' \item{pr_upper_gen}{Upper limit of the studentized bootstrap confidence interval of the estimated probabilities  based on the generating model}
#' \item{pr_lower_best}{Lower limit of the studentized bootstrap confidence interval of the estimated probabilities  based on the best model}
#' \item{pr_upper_best}{Upper limit of the studentized bootstrap confidence interval of the estimated probabilities  based on the best model}
#' \item{haz_lower_gen}{Lower limit of the studentized bootstrap confidence interval of the estimated hazard rates  based on the generating model}
#' \item{haz_upper_gen}{Upper limit of the studentized bootstrap confidence interval of the estimated hazard rates  based on the generating model}
#' \item{haz_lower_best}{Lower limit of the studentized bootstrap confidence interval of the estimated hazard rates  based on the best model}
#' \item{haz_upper_best}{Upper limit of the studentized bootstrap confidence interval of the estimated hazard rates  based on the best model}
#' \item{mu_lower_ma}{Lower limit of model-averaged studentized bootstrap confidence interval of the estimated mean }
#' \item{mu_upper_ma}{Upper limit of model-averaged studentized bootstrap confidence interval of the estimated mean }
#' \item{pr_lower_ma}{Lower limit of model-averaged studentized bootstrap confidence interval of the estimated probabilities  }
#' \item{pr_upper_ma}{Upper limit of model-averaged studentized bootstrap confidence interval of the estimated probabilities  }
#' \item{haz_lower_ma}{Lower limit of model-averaged studentized bootstrap confidence interval of the estimated hazard rates  }
#' \item{haz_upper_ma}{Upper limit of model-averaged studentized bootstrap confidence interval of the estimated hazard rates  }
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
#' alpha <- 0.05 # confidence level
#' y <- 304 # cut-off year for estimating probability
#' B <- 100 # number of bootstraps
#' BB <- 100 # number of double bootstraps
#' which.model <- 2 # specify the generating model
#'
#' # construct confidence invtervals
#' res <- marp::marp_confint(data,m,t,B,BB,alpha,y,which.model)
#' }
#'
#' @export

marp_confint <- function(data,m,t,B,BB,alpha,y,which.model) {
  out <- marp(data,t,m,y,which.model)
  par_hat <- matrix(c(out$par1, out$par2), 6, 2)
  mu_hat <- out$mu_hat
  pr_hat <- out$pr_hat
  haz_hat <- matrix(c(out$haz_hat), length(t), 6)
  best.model <- out$model_best
  weights_aic <- out$weights_AIC
  ## percentile bootstrap confidence interval
  percent <- percent_confint(data,B,t,m,y,which.model)
  ## model-averaged estimates using bootsrtap weights
  mu_bstrp <- mu_hat %*% percent$weights_bstp
  pr_bstrp <- pr_hat %*% percent$weights_bstp
  haz_bstrp <- haz_hat %*% percent$weights_bstp
  out1 <- list("mu_bstrp" = mu_bstrp, "pr_bstrp" = pr_bstrp, "haz_bstrp" = as.numeric(haz_bstrp))
  ## studentized bootstrap confidence interval
  student <- student_confint(n = length(data),B,t,m,BB,par_hat,mu_hat,pr_hat,haz_hat,weights_aic,alpha,y,best.model,which.model)
  return(list("out" = append(out, out1),"percent_CI" = percent,"student_CI" = student))
}
