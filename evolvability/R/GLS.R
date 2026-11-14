#' Generalized least square
#'
#' \code{GLS} utilizes \code{lm.fit} and Cholesky decomposition to fit a
#' generalized least squares regression
#'
#' @param y response variable
#' @param X design matrix
#' @param R residual covariance or correlation matrix (can be sparse), ignored
#'   if \code{L} is provided.
#' @param L lower triangular matrix of the Cholesky decomposition of \code{R}
#'   (optional).
#' @param coef_only reduces the output of the model to the estimated
#'   coefficients (and the generalized residual sums of squares) only.
#' @details Note that the size of \code{R} does not matter (i.e. if \code{R} is
#'   multiplied by a scalar, the results don't change). Note also that the
#'   R-squared is estimated as 1-GSSE/GSST, where GSSE is the generalized
#'   residual sum of squares (i.e. the objective function score of the model)
#'   and GSST is the generalized total sum of squares (i.e. the objective
#'   function score of the model when only the intercept is included in the
#'   model)
#' @return \code{GLS} a \code{\link{list}} of
#' \itemize{
#' \item{\code{coef}: a table of estimates and standard errors}
#' \item{\code{R2}: the R-squared of the model fit}
#' \item{\code{sigma2}: the residual variance}
#' \item{\code{GSSE}: the generalized residual sum of squares (objective function
#'  score)}
#' \item{\code{coef_vcov}: the error variance matrix of the estimates}
#' }
#' @author Geir H. Bolstad
#' @importFrom stats lm.fit
#' @export
GLS <- function(y, X, R = NULL, L = NULL, coef_only = FALSE) {
  if (is.null(L)) {
    L <- t(chol(R))
  }
  if (coef_only) {
    obj <- lm.fit(forwardsolve(L, X), forwardsolve(L, y))
    GSSE <- sum(obj$residuals^2) # generalized residual sum of squares
    output <- list(coef = obj$coef, GSSE = GSSE)
  } else {
    X_star <- forwardsolve(L, X)
    obj <- lm.fit(X_star, forwardsolve(L, y))
    GSSE <- sum(obj$residuals^2) # generalized residual sum of squares
    sigma2 <- GSSE / obj$df.residual # generalized residual variance
    error_var <- c(sigma2) * solve(t(X_star) %*% X_star) # error variance matrix
    X_int <- cbind(X_star[, 1]) 
    GSST <- sum(lm.fit(X_int, forwardsolve(L, y))$residuals^2) # generalized total
    # sum of squares of the intercept only model
    output <- list(
      coef = cbind(Estimate = obj$coefficients, SE = sqrt(diag(error_var))),
      R2 = 1 - GSSE / GSST, sigma2 = sigma2, GSSE = GSSE, coef_vcov = error_var
    )
  }
  return(output)
}
