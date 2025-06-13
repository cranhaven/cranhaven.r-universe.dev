# Methods for the sclr class
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/10/23

#' Print a \code{sclr} object.
#' 
#' Summarises a \code{sclr} object for printing. For a dataframe summary, see
#' \code{\link[=tidy.sclr]{tidy}}.
#' 
#' @param x,object An object returned by \code{\link{sclr}}.
#' @param level Confidence level for the intervals.
#' @param ... Not used. Needed to match generic signature.
#' 
#' @export
print.sclr <- function(x, level = 0.95, ...) summary(x, level = level, ...)

#' @rdname print.sclr
#' @export
summary.sclr <- function(object, level = 0.95, ...) {
  cat("Call: ")
  print(object$call[["formula"]])
  
  cat("\nParameter estimates\n")
  print(coef(object))
  
  cat("\n95% confidence intervals\n")
  print(confint(object, level = level))
  
  cat("\nLog likelihood:", sclr_log_likelihood(object), "\n")
  
  invisible(NULL)
}

#' ML estimate components
#' 
#' \code{coef} returns MLE's.
#' \code{vcov} returns the estimated variance-covariance matrix at MLE's. 
#' \code{confint} returns the confidence interval.
#' \code{model.matrix} returns the model matrix (x).
#' \code{model.frame} returns the model frame (x and y in one matrix).
#' 
#' @param object,formula An object returned by \code{\link{sclr}}.
#' @param parm Parameter name, if missing, all parameters are considered.
#' @param level Confidence level.
#' @param ... Not used. Needed to match generic signature.
#' 
#' @importFrom stats coef vcov
#'
#' @export
coef.sclr <- function(object, ...) object$parameters

#' @rdname coef.sclr
#' @export
vcov.sclr <- function(object, ...) object$covariance_mat

#' @rdname coef.sclr
#' @importFrom stats confint.default
#' @export
confint.sclr <- function(object, parm, level = 0.95, ...) {
  if (is.null(object$parameters)) return(NULL)
  confint.default(object, parm, level, ...)
}

#' @rdname coef.sclr
#' @importFrom stats model.matrix
#' @export
model.matrix.sclr <- function(object, ...) object$x

#' @rdname coef.sclr
#' @importFrom stats model.frame
#' @export
model.frame.sclr <- function(formula, ...) formula$model

#' @rdname coef.sclr
#' @importFrom stats logLik
#' @export
logLik.sclr <- function(object, ...) {
  ll <- object$log_likelihood
  attr(ll, "nobs") <- nrow(model.matrix(object))
  attr(ll, "df") <- length(coef(object))
  class(ll) <- "logLik"
  ll
}

#' Predict method for scaled logit model x.
#' 
#' Returns only the protection estimates. The only supported interval is
#' a confidence interval (i.e. the interval for the estimated expected value).
#' 
#' The model is \deqn{P(Y = 1) = \lambda(1 - logit^{-1}(\beta_0 +
#' \beta_1X_1 + \beta_2X_2 + ... + \beta_kX_k))} Where \eqn{Y} is the binary
#' outcome indicator, (e.g. 1 - infected, 0 - not infected). \eqn{X} - 
#' covariate.
#' \eqn{k} - number of covariates.
#' This function calculates \deqn{\beta_0 + \beta_1X_1 + \beta_2X_2 + ..
#' . + \beta_kX_k} transformations at the covariate values found in 
#' \code{newdata} as well as the variance-covariance matrices of those
#' transformations. This is used to calculate the confidence intervals at the
#' given parameter values. The inverse logit transformation is then applied
#' to point estimates and interval bounds.
#' 
#' @param object Object returned by \code{\link{sclr}}.
#' @param newdata A dataframe with all covariates. Names should be as they
#'   appear in the formula in the call to \code{\link{sclr}}.
#' @param ci_lvl Confidence level for the calculated interval.
#' @param ... Not used. Needed to match generic signature.
#'
#' @return A \code{\link[tibble]{tibble}} obtained by adding the following
#' columns to \code{newdata}:
#' \item{prot_point_lin prot_l_lin prot_u_lin}{Point estimate, low and high 
#' bounds of the linear transformation.}
#' \item{prot_sd_lin}{Estimated standard deviation of the linear 
#' transformation.}
#' \item{prot_point prot_l prot_u}{Inverse logit-transformed 
#' point estimate, low and high bounds of the linear transformation.}
#' 
#' @importFrom stats predict delete.response model.frame model.matrix qnorm
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @importFrom purrr map_dbl
#' 
#' @export
predict.sclr <- function(object, newdata, ci_lvl = 0.95, ...) {
  
  # Covariates
  # Relying on this to throw an error when newdata does not contain
  # the variables that it needs to contain.
  terms_noy <- delete.response(object$terms)
  mf <- model.frame(terms_noy, newdata)
  model_mat <- model.matrix(terms_noy, mf)
  
  # Estimated parameters
  ests <- coef(object)
  ests_beta_mat <- matrix(ests[-1], ncol = 1) # Estimated betas
  
  # Point estimates
  prot_point_lin <- map_dbl(
    1:nrow(model_mat), 
    function(i) matrix(model_mat[i, ], nrow = 1) %*% ests_beta_mat
  )

  # Variance of linear predictor
  ests_beta_cov <- vcov(object)[-1, -1] # Beta covariances
  lin_vars <- map_dbl(
    1:nrow(model_mat), 
    function(i) {
      matrix(model_mat[i, ], nrow = 1) %*% 
        ests_beta_cov %*% 
        matrix(model_mat[i, ], ncol = 1)
    }
  )
  sds <- sqrt(lin_vars)
  
  # Ranges
  lvl <- qnorm((1 + ci_lvl) / 2)
  prot_l_lin <- prot_point_lin - lvl * sds
  prot_u_lin <- prot_point_lin + lvl * sds
  
  # Collate
  predret <- tibble(
    prot_point_lin = prot_point_lin,
    prot_sd_lin = sds,
    prot_l_lin = prot_l_lin,
    prot_u_lin = prot_u_lin,
    prot_point = invlogit(prot_point_lin),
    prot_l = invlogit(prot_l_lin),
    prot_u = invlogit(prot_u_lin)
  )
  bind_cols(predret, newdata)
}

#' Tidy a \code{sclr} object.
#' 
#' Summarises the objects returned by \code{\link{sclr}} 
#' into a \code{\link[tibble]{tibble}}.
#'
#' @param x An object returned by \code{\link{sclr}}.
#' @param ci_level Confidence level for the intervals.
#' @param ... Not used. Needed to match generic signature.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per model parameter. 
#' Columns:
#' \item{term}{Name of model parameter.}
#' \item{estimate}{Point estimate.}
#' \item{std_error}{Standard error.}
#' \item{conf_low}{Lower bound of the confidence interval.}
#' \item{conf_high}{Upper bound of the confidence interval.}
#' 
#' @importFrom broom tidy
#' @importFrom dplyr inner_join
#' 
#' @export
tidy.sclr <- function(x, ci_level = 0.95, ...) {
  pars <- tibble(
    term = names(coef(x)),
    estimate = coef(x),
    std_error = sqrt(diag(vcov(x)))
  )
  cis <- confint(x, level = ci_level)
  cisdf <- tibble(
    term = rownames(cis), conf_low = cis[, 1], conf_high = cis[, 2]
  )
  inner_join(pars, cisdf, by = "term")
}
