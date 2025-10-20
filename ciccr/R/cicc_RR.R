#' @title Causal Inference on Relative Risk
#'
#' @description Provides upper bounds on the average of log relative risk
#' under the monotone treatment response (MTR) and monotone treatment selection (MTS) assumptions.
#'
#' @param y n-dimensional vector of binary outcomes
#' @param t n-dimensional vector of binary treatments
#' @param x n by d matrix of covariates
#' @param sampling 'cc' for case-control sampling; 'cp' for case-population sampling; 'rs' for random sampling (default =  'cc')
#' @param cov_prob coverage probability of a uniform confidence band (default = 0.95)
#'
#' @return An S3 object of type "ciccr". The object has the following elements:
#' \item{est}{estimates of the upper bounds on the average of log relative risk at p=0 and p=1}
#' \item{se}{pointwise standard errors at p=0 and p=1}
#' \item{ci}{the upper end points of the uniform confidence band at p=0 and p=1}
#' \item{pseq}{two end points: p=0 and p=1}
#'
#' @examples
#' # use the ACS_CC dataset included in the package.
#'   y = ciccr::ACS_CC$topincome
#'   t = ciccr::ACS_CC$baplus
#'   x = ciccr::ACS_CC$age
#'   results_RR = cicc_RR(y, t, x, sampling = 'cc', cov_prob = 0.95)
#'
#' @references Jun, S.J. and Lee, S. (2023). Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions.
#' \url{https://arxiv.org/abs/2004.08318}.
#' @references Manski, C.F. (1997). Monotone Treatment Response.
#' Econometrica, 65(6), 1311-1334.
#' @references Manski, C.F. and Pepper, J.V. (2000). Monotone Instrumental Variables: With an Application to the Returns to Schooling.
#' Econometrica, 68(4), 997-1010.
#'
#' @export
cicc_RR = function(y, t, x, sampling = 'cc', cov_prob = 0.95){

  # Check whether sampling is case-control, case-population, or random
  if ( sum( !(sampling %in% c('cc','cp','rs')) ) > 0 ){
    stop("'sampling' must be 'cc', 'cp', or 'rs'.")
  }

  if (sampling=='cc'){

    results = avg_RR_logit(y, t, x, 'case')
    est_case = results$est
    se_case = results$se

    results = avg_RR_logit(y, t, x, 'control')
    est_control = results$est
    se_control = results$se

    cv_norm = stats::qnorm(1-(1-cov_prob)/2) # two-sided normal critical value

    # Uniform confidence band for the upper bounds under case-control studies
    est = c(est_control, est_case)
    se = c(se_control, se_case)
    ci = est + cv_norm*max(se)

  }  else if (sampling=='cp'){

    results = avg_RR_logit(y, t, x, 'control')
    est = rep(results$est,2)
    se = rep(results$se,2)
    ci = est + stats::qnorm(cov_prob)*se

  }  else if (sampling=='rs'){

    results = avg_RR_logit(y, t, x, 'all')
    est = rep(results$est,2)
    se = rep(results$se,2)
    ci = est + stats::qnorm(cov_prob)*se
  }

  outputs = list("est" = est, "se" = se, "ci" = ci, "pseq" = c(0,1), "cov_prob" = cov_prob)

  class(outputs) = "ciccr"

  outputs
}
