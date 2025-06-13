# sclr class functions
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/11/04

#' Fits the scaled logit model
#'
#' Used to fit the scaled logit model from Dunning (2006).
#'
#' The model is logistic regression with an added parameter for the top
#' asymptote. That parameter is reported as \code{theta} (or \code{(Baseline)}
#' if \code{conventional_names = TRUE}). Note that it is reported on the logit
#' scale. See \code{vignette("sclr-math")} for model specification,
#' log-likelihood, scores and second derivatives. The main default optimisation
#' algorithm is Newton-Raphson. Gradient ascent is used as a fallback by
#' default. Computing engine behind the fitting is \code{\link{sclr_fit}}.
#'
#' @param formula an object of class "formula": a symbolic description of the
#'   model to be fitted.
#' @param data a data frame.
#' @param ci_lvl Confidence interval level for the parameter estimates.
#' @inheritParams sclr_fit
#'
#' @return An object of class \code{sclr}. This is a list with the following
#'   elements:
#'
#'   \item{parameters}{Maximum likelihood estimates of the parameter values.}
#'
#'   \item{covariance_mat}{The variance-covariance matrix of the parameter
#'   estimates.}
#'   
#'   \item{algorithm}{Algorithm used.}
#'   
#'   \item{algorithm_return}{Everything the algorithm returned.}
#'
#'   \item{n_converge}{The number of Newton-Raphson iterations (including
#'   resets) that were required for convergence.}
#'
#'   \item{x}{Model matrix derived from \code{formula} and \code{data}.}
#'
#'   \item{y}{Response matrix derived from \code{formula} and \code{data}.}
#'
#'   \item{call}{The original call to \code{sclr}.}
#'
#'   \item{model}{Model frame object derived from \code{formula} and
#'   \code{data}.}
#'
#'   \item{terms}{Terms object derived from model frame.}
#'
#'   \item{ci}{Confidence intervals of the parameter estimates.}
#'
#'   \item{log_likelihood}{Value of log-likelihood calculated at the ML
#'   estimates of parameters.}
#'   
#'   \item{formula}{Passed formula.}
#'   
#'   \item{data}{Passed data.}
#'
#'   Methods supported: \code{\link[=print.sclr]{print}},
#'   \code{\link[=vcov.sclr]{vcov}}, \code{\link[=coef.sclr]{coef}},
#'   \code{\link[=model.frame.sclr]{model.frame}},
#'   \code{\link[=model.matrix.sclr]{model.matrix}},
#'   \code{\link[=summary.sclr]{summary}}, \code{\link[=predict.sclr]{predict}},
#'   \code{\link[=tidy.sclr]{tidy}} (\code{\link{broom}} package),
#'   \code{\link[=logLik.sclr]{logLik}}.
#'
#' @references Dunning AJ (2006). "A model for immunological correlates of
#'   protection." Statistics in Medicine, 25(9), 1485-1497.
#'   \url{https://doi.org/10.1002/sim.2282}.
#'
#' @examples
#' library(sclr)
#' fit1 <- sclr(status ~ logHI, one_titre_data)
#' summary(fit1)
#' @importFrom stats confint model.frame model.matrix model.response
#' @importFrom rlang abort warn
#'
#' @export
sclr <- function(formula, data = NULL, 
                 ci_lvl = 0.95, 
                 tol = 10^(-7),
                 algorithm = c("newton-raphson", "gradient-ascent"),
                 nr_iter = 2e3, ga_iter = 2e3, n_conv = 3,
                 conventional_names = FALSE, seed = NULL) {
  
  if (!inherits(formula, "formula") || missing(formula)) 
    abort("must supply a formula")

  cl <- match.call()
  mf <- model.frame(formula, data, drop.unused.levels = TRUE)

  # Design matirix
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  if (ncol(x) == 1) warn("no unique solution with no covariates")

  # Response vector
  y <- model.response(mf)
  if (!all(y %in% c(0, 1))) abort("response should be a vector with 0 and 1")
  if (!is.numeric(y)) abort("response should be numeric")

  # Actual model fit
  fit <- sclr_fit(
    y, x, tol, algorithm, nr_iter, ga_iter, n_conv, conventional_names, seed
  )

  # Build the return list
  fit <- new_sclr(fit, x, y, cl, mf, mt)
  fit$ci <- confint(fit, level = ci_lvl)
  fit$log_likelihood <- sclr_log_likelihood(fit)
  fit$formula <- formula
  fit$data <- data
  fit
}

#' Create a new \code{sclr} object
#' 
#' \code{new_sclr} creates the object \code{\link{sclr}} returns.
#' \code{is_sclr} checks if the object is of class \code{sclr}.
#'
#' @param fit A list returned by \code{\link{sclr_fit}}.
#' @param x Model matrix.
#' @param y Model response.
#' @param cl Call.
#' @param mf Model frame.
#' @param mt Model terms.
#'
#' @return \code{sclr} object
#' @export
new_sclr <- function(fit, x, y, cl, mf, mt) {
  stopifnot(is.list(fit))
  stopifnot(is.matrix(x))
  stopifnot(is.numeric(y))
  stopifnot(is.call(cl))
  stopifnot(is.data.frame(mf))
  stopifnot(inherits(mt, "terms"))
  fit <- c(fit, list(
    x = x, y = y,
    call = cl, model = mf, terms = mt
  ))
  class(fit) <- "sclr"
  fit
}

#' @rdname new_sclr
#' @export
is_sclr <- function(fit) "sclr" %in% class(fit)
