#' Fitted values for a subset of active variables
#' 
#' @description
#' \code{\link{fitted_values_cv}} extracts the active set from \eqn{X^TX}{"X'X"} and
#' \eqn{X^Ty}{"X'y"} to get out-of-sample predictions
#' for a matrix already containing only the active variables.
#' The function is only relevant for cases where at least one variable is selected.
#'
#' @param XtX_all Cross product of all covariates
#' @param Xty_all Cross product of covariates and outcome
#' @param x_pred Covariates matrix of the prediction sample
#' @param nm_act Names of active variables
#'
#' @return Fitted values in the prediction sample.
#'
#' @keywords internal
#'
fitted_values_cv = function (XtX_all,Xty_all,x_pred,nm_act) {
  # Extract relevant rows and columns
  XtX = XtX_all[nm_act,nm_act]
  Xty = Xty_all[nm_act,]
  
  # Calculate point estimates in estimation sample
  b = tryCatch(solve(XtX, Xty), error=function(e) NULL) # much faster than standard solve
  if (is.null(b)) {        # In case b has not been estimated
    fit_val = b
  } else {
    # Calculate fitted values in prediction sample
    fit_val = x_pred%*%b
  }
  return(fit_val)
}


#' Normalization of sample weights for potential sample weights
#' 
#' @description
#' \code{\link{norm_w_to_n}} normalizes weights either to N or to N in treated and controls separately.
#' 
#' @param w Vector or n x 1 matrix of weights that should be normalized
#' @param d Vector of treatment indicators
#'
#' @return Normalized weights.
#'
#' @keywords internal
#'
norm_w_to_n = function(w,d=NULL) {
  if (is.numeric(w) & (is.numeric(w) | is.matrix(w))){
    w = as.matrix(w)
    # Check for negative weights
    if (any(w < 0)) {
      min_pos_weight = min(w[w >= 0])
      w[w < 0] = min_pos_weight
      warning("Negative weights were replaced by the minimum non-negative weight value")
    }
    if (is.null(d)) {
      w = w / sum(w) * nrow(w)
    } else {
      # Separate weights of treated and controls
      w1 = w * d
      w0 = w * (1-d)
      # Normalize weights to sum to N in both groups
      w1 = w1 / sum(w1) * nrow(w)
      w0 = w0 / sum(w0) * nrow(w)
      # Unify weights again
      w = w1 + w0
      return(w)
    }
  } else {
    stop("Weights do not have the required format (vector or n x 1 matrix)")
  }
}


#' Helper function to find the position for prespecified SE rules
#' 
#' @description
#' \code{\link{find_Xse_ind}} is a helper function that finds the position for prespecified SE rules.
#'
#' @param CV Vector of cross-validated criterion
#' @param ind_min Index of cross-validated minimum
#' @param oneSE Vector that contains the standard errors of the cross-validated criterion for the whole grid
#' @param factor Factor in which direction to go: Negative values favor smaller models, positive values favor larger models
#'
#' @return Index on the Lambda grid.
#'
#' @keywords internal
#'
find_Xse_ind = function(CV,ind_min,oneSE,factor) {
  cv_temp = CV - (CV[ind_min] + abs(factor) * oneSE[ind_min])
  if (factor < 0) {
    if (ind_min == 1) return(ind_min)
    for (i in ind_min:1) {
      ind = i+1
      if (cv_temp[i] < 0) next
      else if (cv_temp[i] > 0) break
    }
  } else if (factor > 0) {
    if (ind_min == length(CV)) return(ind_min)
    for (i in ind_min:length(CV)) {
      ind = i-1
      if (cv_temp[i] < 0) next
      else if (cv_temp[i] > 0) break
    }
  } else ind = ind_min
  return(ind)
}


#' Adds an intercept to a matrix
#' 
#' @description
#' \code{\link{add_intercept}} adds an intercept to a matrix.
#' 
#' @param mat Any matrix (with column names).
#'
#' @return Matrix with intercept.
#' 
#' @keywords internal
#'
add_intercept <- function(mat) {
  if (is.null(dim(mat))) {
    mat = as.matrix(mat,ncol=1)
    colnames(mat) = "Var1"
  }
  
  if (all(mat[,1] == 1)) {
    colnames(mat)[1] == "(Intercept)"
    return(mat)
  } else {
    mat = cbind(rep(1,nrow(mat)),mat)
    colnames(mat)[1] = "(Intercept)"
    return(mat)
  }
}


#' Sanitizes potential sample weights
#' 
#' @description
#' \code{\link{handle_weights}} cleans potential sample weights or codes them as ones if they are not specified.
#' 
#' @param w Vector or n x 1 matrix of weights or null if no weights provided
#' @param n Number of observations
#'
#' @return Vector of weights.
#'
#' @keywords internal
#'
handle_weights = function(w,n) {
  # Create weights of ones if no weights are specified
  if (is.null(w)) {
    w = as.matrix(rep(1,n),nrow=n,ncol=1)
  } else {
    w = as.matrix(w,nrow=n,ncol=1)
  }
  colnames(w) = "w"
  
  # Normalize the weights
  w = norm_w_to_n(w)
  return(w)
}


#' plasso fitting
#' 
#' @description
#' \code{\link{fit_betas}} estimates OLS model only for active coefficients (from lasso)
#' 
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y Vector of outcomes
#' @param w Vector of weights
#' @param nm_act Vector of active variables
#' @param coef_lasso Vector of lasso coefficients
#' 
#' @return Beta estimates.
#'
#' @keywords internal
#'
fit_betas = function(x,y,w,nm_act,coef_lasso) {
  if (length(nm_act) == 1){
    xact_w = x[,nm_act] * sqrt(w)
    xact_w = as.matrix(xact_w)
  } else {
    xact = x[,nm_act]
    xact_w = apply(xact,2,`*`,sqrt(w))
  }
  
  y_w = y * sqrt(w)
  XtX = crossprod(xact_w)
  Xty = crossprod(xact_w,y_w)

  beta_plasso = solve(XtX, Xty)
  beta_plasso = unlist(beta_plasso[,1])
  coef_plasso = coef_lasso
  coef_plasso[nm_act] = beta_plasso
  return(coef_plasso)
}