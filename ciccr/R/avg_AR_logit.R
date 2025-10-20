#' @title An Average of the Upper Bound on Causal Attributable Risk
#'
#' @description Averages the upper bound on causal attributable risk using prospective and retrospective logistic regression models
#' under the monotone treatment response (MTR) and monotone treatment selection (MTS) assumptions.
#'
#' @param y n-dimensional vector of binary outcomes
#' @param t n-dimensional vector of binary treatments
#' @param x n by d matrix of covariates
#' @param sampling 'cc' for case-control sampling; 'cp' for case-population sampling; 'rs' for random sampling (default =  'cc')
#' @param p_upper specified upper bound for the unknown true case probability (default = 1)
#' @param length specified length of a sequence from 0 to p_upper (default = 21)
#' @param interaction TRUE if there are interaction terms in the retrospective logistic model; FALSE if not (default = TRUE)
#' @param eps a small constant that determines the trimming of the estimated probabilities.
#' Specifically, the estimate probability is trimmed to be between eps and 1-eps (default = 1e-8).
#'
#' @return An S3 object of type "ciccr". The object has the following elements.
#' \item{est}{(length)-dimensional vector of the average of the upper bound of causal attributable risk}
#' \item{pseq}{(length)-dimensional vector of a grid from 0 to p_upper}
#'
#' @examples
#' # use the ACS_CC dataset included in the package.
#'   y = ciccr::ACS_CC$topincome
#'   t = ciccr::ACS_CC$baplus
#'   x = ciccr::ACS_CC$age
#'   results = avg_AR_logit(y, t, x, sampling = 'cc')
#'
#' @references Jun, S.J. and Lee, S. (2023). Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions.
#' \url{https://arxiv.org/abs/2004.08318}.
#' @references Manski, C.F. (1997). Monotone Treatment Response.
#' Econometrica, 65(6), 1311-1334.
#' @references Manski, C.F. and Pepper, J.V. (2000). Monotone Instrumental Variables: With an Application to the Returns to Schooling.
#' Econometrica, 68(4), 997-1010.
#'
#' @export
avg_AR_logit = function(y, t, x, sampling = 'cc', p_upper = 1L, length = 21L, interaction = TRUE, eps=1e-8){

  # Check whether y is either 0 or 1
  if ( sum( !(y %in% c(0,1)) ) > 0 ){
    stop("Each element of 'y' must be either 0 or 1.")
  }

  # Check whether t is either 0 or 1
  if ( sum( !(t %in% c(0,1)) ) > 0 ){
    stop("Each element of 't' must be either 0 or 1.")
  }

  # Check whether sampling is case-control, case-population, or random
  if ( sum( !(sampling %in% c('cc','cp','rs')) ) > 0 ){
    stop("'sampling' must be 'cc', 'cp', or 'rs'.")
  }

  # grid points for p
  pgrd = seq(from = 0, to = p_upper, length.out = ceiling(length))
  pseq = matrix(pgrd, nrow=1)

### Estimation of the upper bound on AR under case-control and case-population sampling

if (sampling!='rs'){

  # Prospective logistic estimation of P(Y=1|X=x)

  lm_pro = stats::glm(y~x, family=stats::binomial("logit"))
  est_pro = stats::coef(lm_pro)
  fit_pro = stats::fitted.values(lm_pro)
  fit_pro = matrix(fit_pro,ncol=1)

  fit_pro = trim_pr(fit_pro)

  # Estimation of r(x,p)

  hhat = mean(y)

  if (sampling=='cc'){
    r_num = ((1-hhat)*fit_pro)%*%pseq
    r_den = r_num + (hhat*(1-fit_pro))%*%(1-pseq)
    r_cc = r_num/r_den
  }  else if (sampling=='cp'){
    r1 = (1-hhat)/hhat
    r2 = fit_pro/(1-fit_pro)
    r_cp = (r1*r2)%*%pseq
  }

  # Retrospective logistic estimation of P(T=1|Y=y,X=x)

  if (interaction == TRUE){

    lm_ret = stats::glm(t~y+x+y:x, family=stats::binomial("logit"))
    est_ret = stats::coef(lm_ret)
    x_reg_y1 = cbind(1,1,x,1*x)
    x_reg_y0 = cbind(1,0,x,0*x)

  } else if (interaction == FALSE){

    lm_ret = stats::glm(t~y+x, family=stats::binomial("logit"))
    est_ret = stats::coef(lm_ret)
    x_reg_y1 = cbind(1,1,x)
    x_reg_y0 = cbind(1,0,x)
  }

  fit_ret_y1 = exp(x_reg_y1%*%est_ret)/(1+exp(x_reg_y1%*%est_ret))
  fit_ret_y0 = exp(x_reg_y0%*%est_ret)/(1+exp(x_reg_y0%*%est_ret))

  # Estimation of Gamma_AR(x,p)

  P11 = trim_pr(fit_ret_y1)
  P10 = trim_pr(fit_ret_y0)
  P11 = matrix(P11, ncol=1)%*%matrix(1, nrow=1, ncol=ncol(pseq))
  P10 = matrix(P10, ncol=1)%*%matrix(1, nrow=1, ncol=ncol(pseq))

  P01 = 1 - P11
  P00 = 1 - P10

  # Estimation of beta_AR(p,y)

  if (sampling=='cc'){

    term1_cc_den = P10 + r_cc*(P11-P10)
    term1_cc = P11/term1_cc_den

    term2_cc_den = P00 + r_cc*(P01-P00)
    term2_cc = P01/term2_cc_den

    GammaAR_cc = term1_cc - term2_cc

    betaAR_cc1 = colMeans(r_cc*GammaAR_cc*y)/mean(y)
    betaAR_cc0 = colMeans(r_cc*GammaAR_cc*(1-y))/mean(1-y)
    betaAR_cc = pgrd*betaAR_cc1 + (1-pgrd)*betaAR_cc0
    est = betaAR_cc

  }  else if (sampling=='cp'){

    term1_cp = P11/P10
    term2_cp = P01/P00

    GammaAR_cp = term1_cp - term2_cp
    betaAR_cp = colMeans(r_cp*GammaAR_cp*(1-y))/mean(1-y)
    est = betaAR_cp

  }

}

### Estimation of the upper bound on AR under random sampling

  if (sampling=='rs'){

    # Prospective logistic estimation of P(Y=1|T=t,X=x)

    if (interaction == TRUE){

      lm_pro = stats::glm(y~t+x+t:x, family=stats::binomial("logit"))
      est_pro = stats::coef(lm_pro)
      x_reg_y1 = cbind(1,1,x,1*x)
      x_reg_y0 = cbind(1,0,x,0*x)

    } else if (interaction == FALSE){

      lm_pro = stats::glm(y~t+x, family=stats::binomial("logit"))
      est_pro = stats::coef(lm_pro)
      x_reg_y1 = cbind(1,1,x)
      x_reg_y0 = cbind(1,0,x)
    }

    fit_pro_y1 = exp(x_reg_y1%*%est_pro)/(1+exp(x_reg_y1%*%est_pro))
    fit_pro_y0 = exp(x_reg_y0%*%est_pro)/(1+exp(x_reg_y0%*%est_pro))

    est = mean(fit_pro_y1) - mean(fit_pro_y0)
    est = rep(est, length)
  }

  outputs = list("est" = est, "pseq" = pgrd)

class(outputs) = "ciccr"

outputs

}
