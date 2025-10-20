#' @title Average Adjusted Association
#'
#' @description Averages the log odds ratio using prospective or retrospective high-dimensional logistic regression
#'
#' @param y n-dimensional vector of binary outcomes
#' @param t n-dimensional vector of binary treatments
#' @param x n by d matrix of covariates
#' @param type 'pro' if the average is based on prospective regression;
#' 'retro' if it is based on prospective regression (default = 'pro')
#' @param k number of folds in k-fold partition (default = 10)
#' @return An S3 object of type "ciccr". The object has the following elements.
#' \item{est}{a scalar estimate}
#' \item{se}{standard error}
#'
#' @examples
#' # use the ACS dataset included in the package
#'   y = ciccr::ACS$topincome
#'   t = ciccr::ACS$baplus
#'   age = ciccr::ACS$age
#'   x = splines::bs(age, df=6) # b-splines for age
#'
#'   results = AAA_DML(y, t, x, 'pro', k=2)
#'
#' @references Jun, S.J. and Lee, S. (2020). Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions.
#' \url{https://arxiv.org/abs/2004.08318}.
#' @export
AAA_DML = function(y, t, x, type = 'pro', k = 10){

  # redefine variables

  X = x
  K = k

  # Type of regression
  if (type=='pro'){
      outcome = y
      treat = t
  }  else if (type=='retro'){
      outcome = t
      treat = y
  }
  else {
    stop("'type' must be either 'pro' or 'retro'.")
  }

  # Check whether y is either 0 or 1
  if ( sum( !(y %in% c(0,1)) ) > 0 ){
    stop("Each element of 'y' must be either 0 or 1.")
  }

  # Check whether t is either 0 or 1
  if ( sum( !(t %in% c(0,1)) ) > 0 ){
    stop("Each element of 't' must be either 0 or 1.")
  }

  # Construct k-fold partition using random splitting

  unif_tmp = stats::runif(length(outcome), min = 0, max = K)
  kfoldindex = ceiling(unif_tmp)

  n_kfold = {}

  for(k_j in 1:K){

    n_main = length(outcome[kfoldindex==k_j])
    n_kfold = c(n_kfold, n_main)
  }
  n_kmax = max(n_kfold)

  est_matrix = matrix(0,nrow=n_kmax,ncol=K)

  for(k_j in 1:K){

    Y_main = outcome[kfoldindex==k_j]
    Y_est = outcome[kfoldindex!=k_j]

    T_main = treat[kfoldindex==k_j]
    T_est = treat[kfoldindex!=k_j]

    Y_case = Y_est[T_est==1]
    Y_control = Y_est[T_est==0]

    if (is.matrix(X)==TRUE){
      X_main = X[kfoldindex==k_j,]
      X_est = X[kfoldindex!=k_j,]
      X_case = X_est[T_est==1,]
      X_control = X_est[T_est==0,]
    } else {
      stop("'x' must be a matrix with at least two columns.")
    }

    # Estimation of Pr(outcome=1|X=x,treat=1) using glmnet package

    cvfit_case = glmnet::cv.glmnet(X_case, Y_case, family = "binomial", type.measure = "deviance")
    Pr_Y_case = stats::predict(cvfit_case, newx = X_main, s = "lambda.min", type = "response")

    # Estimation of Pr(outcome=1|X=x,treat=0) using glmnet package

    cvfit_control = glmnet::cv.glmnet(X_control, Y_control, family = "binomial", type.measure = "deviance")
    Pr_Y_control = stats::predict(cvfit_control, newx = X_main, s = "lambda.min", type = "response")

    # Estimation of Pr(treat=1|X=x) using glmnet package

    cvfit_T = glmnet::cv.glmnet(X_est, T_est, family = "binomial", type.measure = "deviance")
    Pr_T = stats::predict(cvfit_T, newx = X_main, s = "lambda.min", type = "response")

    # Conditional odds ratio

    OR = (Pr_Y_case*(1-Pr_Y_control))/((1-Pr_Y_case)*Pr_Y_control)
    term1 = log(OR)
    term2 = (T_main/Pr_T)*(Y_main - Pr_Y_case)/(Pr_Y_case*(1-Pr_Y_case))
    term3 = -((1-T_main)/(1-Pr_T))*(Y_main - Pr_Y_control)/(Pr_Y_control*(1-Pr_Y_control))
    est = term1  + term2 + term3

    est_matrix[1:nrow(est),k_j] = est

  }

  est = mean(colSums(est_matrix)/n_kfold)
  avar = mean(colSums((est_matrix-est)^2)/n_kfold)
  se = sqrt(avar/length(outcome))

  outputs = list("est"=est,"se"=se)
  class(outputs) = 'ciccr'

  outputs
}
