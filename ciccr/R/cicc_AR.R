#' @title Causal Inference on Attributable Risk
#'
#' @description Provides an upper bound on the average of attributable risk
#' under the monotone treatment response (MTR) and monotone treatment selection (MTS) assumptions.
#'
#' @param y n-dimensional vector of binary outcomes
#' @param t n-dimensional vector of binary treatments
#' @param x n by d matrix of covariates
#' @param sampling 'cc' for case-control sampling; 'cp' for case-population sampling; 'rs' for random sampling (default =  'cc')
#' @param p_upper a specified upper bound for the unknown true case probability (default = 1)
#' @param cov_prob coverage probability of a confidence interval (default = 0.95)
#' @param length specified length of a sequence from 0 to p_upper (default = 21)
#' @param interaction TRUE if there are interaction terms in the retrospective logistic model; FALSE if not (default = TRUE)
#' @param no_boot number of bootstrap repetitions to compute the confidence intervals (default = 0)
#' @param eps a small constant that determines the trimming of the estimated probabilities.
#' Specifically, the estimate probability is trimmed to be between eps and 1-eps (default = 1e-8).
#'
#' @return An S3 object of type "ciccr". The object has the following elements:
#' \item{est}{(length)-dimensional vector of the upper bounds on the average of attributable risk}
#' \item{ci}{(length)-dimensional vector of the upper ends of pointwise one-sided confidence intervals}
#' \item{pseq}{(length)-dimensional vector of a grid from 0 to p_upper}
#' \item{cov_prob}{the nominal coverage probability}
#' \item{return_code}{status of existence of missing values in bootstrap replications}
#'
#' @examples
#' # use the ACS_CC dataset included in the package.
#'   y = ciccr::ACS_CC$topincome
#'   t = ciccr::ACS_CC$baplus
#'   x = ciccr::ACS_CC$age
#'   results_AR = cicc_AR(y, t, x, sampling = 'cc', no_boot = 100)
#'
#' @references Jun, S.J. and Lee, S. (2020). Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions.
#' \url{https://arxiv.org/abs/2004.08318}.
#' @references Manski, C.F. (1997). Monotone Treatment Response.
#' Econometrica, 65(6), 1311-1334.
#' @references Manski, C.F. and Pepper, J.V. (2000). Monotone Instrumental Variables: With an Application to the Returns to Schooling.
#' Econometrica, 68(4), 997-1010.
#'
#' @export
cicc_AR = function(y, t, x, sampling = 'cc', p_upper = 1L, cov_prob = 0.95, length = 21L, interaction = TRUE, no_boot = 0L, eps=1e-8){

  # Check whether p_upper is in (0,1]
  if (p_upper <=0L || p_upper > 1L){
    stop("'p_upper' must be in (0,1].")
  }

  # Check whether length > 0
  if (length <= 0){
    stop("'length' must be greater than zero.")
  }

  n = length(y)
  results = avg_AR_logit(y, t, x, sampling=sampling, p_upper=p_upper, length=length, interaction=interaction, eps=eps)
  est = results$est
  est = est*(est <= 1) + 1*(est > 1) # truncated at 1 since AR cannot be larger than 1

  pseq = results$pseq

  if (no_boot > 0){

  data = cbind(y,t,x)
  bt_est_matrix = {}

  for (k in 1:no_boot){

  bt_i = sample.int(n,n,replace=TRUE)
  bt_data = data[bt_i,]
  bt_y = bt_data[bt_i,1]
  bt_t = bt_data[bt_i,2]
  bt_x = bt_data[bt_i,c(-1,-2)]

  bt_results = avg_AR_logit(bt_y, bt_t, bt_x, sampling=sampling, p_upper=p_upper, length=length, interaction=interaction, eps=eps)
  bt_est = bt_results$est
  bt_est_matrix = rbind(bt_est_matrix,bt_est)
  }

  if ( sum(is.na(bt_est_matrix)==TRUE) > 0 ){
    bt_est_matrix = stats::na.omit(bt_est_matrix)
    dropout_rate = round(100*(1 - nrow(bt_est_matrix)/no_boot))
    return_code = paste("Warning: ", dropout_rate, "% of bootstrap samples with missing values are dropped", sep="")
  } else{
    return_code = "Success: no bootstrap sample is dropped"
  }

  tmp = (bt_est_matrix <= matrix(1,nrow=nrow(bt_est_matrix),ncol=1)%*%matrix(est,nrow=1))
  pstar = apply(tmp, 2, mean)
  zstar = stats::qnorm(pstar)
  z0 = stats::qnorm(cov_prob)
  bc_prob = stats::pnorm(z0 + 2*zstar)
  bt_ci = apply(bt_est_matrix, 2, stats::quantile, prob=bc_prob) # bias-corrected percentile method
  bt_ci = diag(bt_ci)

  bt_ci = bt_ci*(bt_ci <= 1) + 1*(bt_ci > 1) # truncated at 1 since AR cannot be larger than 1
  }

  else if (no_boot == 0){

  bt_ci = NA
  return_code = "Only point estimates are provided without bootstrap inference"
  }

  outputs = list("est" = est, "ci" = bt_ci, "pseq" = pseq, "cov_prob" = cov_prob, "return_code" = return_code)

  class(outputs) = "ciccr"

  outputs
}
