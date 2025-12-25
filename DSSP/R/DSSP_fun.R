##  Wrapper function takes X,y,num_samples and prior for eta and returns samples from joint posterior
##  DSSP (Direct Sampling Spatial Prior)
#' Wrapper function to draw samples from the Direct Sampling Spatial Prior (DSSP) model
#'
#' This function samples from the log-posterior of all parameters in the model and returns a list
#'    object containing the samples. It performs a few compatibility checks on the inputs, then
#'    calls the sample.eta(), sample.delta(), and sample.nu().
#' @param formula a two sided linear formula with the response on left and the covariates on the right.
#' @param data a \code{data.frame} or \code{sp::SpatialPointsDataFrame} containing the response variable, covariates and coordinates.
#' @param N is the number of random samples to be drawn from the joint posterior for eta, delta, and nu.
#' @param pars a vector of the prior shape and rate parameters for the inverse-gamma
#' prior distribution of delta, the variance parameter for the Gaussian likelihood.
#' @param log_prior a function evaluating the log of the prior density of eta. Default to be \code{function(x) -x}.
#' @param coords spatial coordinates passed as the \code{value} argument to \code{sp::coordinates()}.
#' @return A list containing N samples of nu, eta, delta, and the original data X and Y.
#' @details
#'  The direct sampling spatial prior model assumes that the spatial model can be written
#'  as the likelihood parameterised with mean vector nu and variance delta
#'  \deqn{(y | nu, delta) ~ N(nu, delta * I)}
#'  where I is the identity matrix.  The prior for the vector of spatial effects nu is
#'  improper but is proportional to
#'  \deqn{\pi(nu | eta) propto (det(M)/2\pi)^{1/2} * exp(-eta nu' M nu/2),}
#'  the prior for delta is assumed to be a inverse-gamma distribution
#'  \deqn{(delta) ~ IG(a,b)}
#'  and the prior for eta can be specified for the user as any valid density function for eta > 0.
#' @export
#' @examples
#' ## Use the Meuse River dataset from the package 'gstat'
#'
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#'
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#'
#' ## Draw 100 samples from the posterior of eta given the data y.
#' OUTPUT <- DSSP(
#'   formula = log(zinc) ~ 1, data = meuse.all, N = 100,
#'   pars = c(0.001, 0.001), log_prior = f
#' )
DSSP <- function(formula, data, N, pars, log_prior = function(x) -x, coords = NULL) {
  stopifnot(is.function(log_prior))

  if (all(class(data) != "SpatialPointsDataFrame")) {
    sp::coordinates(data) <- coords
    coords <- sp::coordinates(data)
  } else {
    if (!is.null(coords)) message("obtaining spatial coordinates from data; ignoring coords provided")
    coords <- sp::coordinates(data)
  }

  w <- sp::coordinates(data)
  if (any(!grepl("scaled", names(attributes(w))))) {
    w <- scale(w)
    coord_scaling <- list(
      center = attr(w, "scaled:center"),
      scale = attr(w, "scaled:scale")
    )
  } else {
    coord_scaling <- list(center = NA, scale = NA)
  }

  mt <- stats::terms(formula, data = data)
  mf <- stats::lm(formula, data = data, method = "model.frame")
  nobs <- nrow(stats::na.omit(mf))
  dep_var <- names(mf)[[1]]
  y <- stats::model.extract(mf, "response")
  y <- scale(y)
  y_scaling <- list(
    center = attr(y, "scaled:center"),
    scale = attr(y, "scaled:scale")
  )
  x <- stats::model.matrix(mt, mf)

  N <- as.integer(N)
  X <- as.matrix(w)
  Y <- as.numeric(y)
  n <- length(y)
  pars <- as.numeric(pars)
  delta <- numeric(length = N)
  eta <- numeric(length = N)

  ##  Declare dimensional constants
  n <- length(Y)
  d <- ncol(X) + 1
  ND <- n - d

  ##  Compute M
  M.list <- make.M(X, covariates = x) ##  Only Needs to return the eigenvalues and vectors
  M <- M.list$M

  EV <- M.list$M.eigen$values
  V <- M.list$M.eigen$vectors
  Q <- crossprod(Y, V)

  ## sample eta
  eta <- sample.eta(N, ND, EV, Q, log_prior, UL = 1000)

  ##  sample delta
  delta <- sample.delta(eta, ND, EV, Q, pars)

  ## sample nu
  nu <- sample.nu(Y, eta, delta, EV, V)
  y_fitted <- nu * y_scaling$scale + y_scaling$center

  dssp.out <- list(
    eta = eta,
    delta = delta,
    nu = nu,
    y_fitted = y_fitted,
    covariates_posterior = M.list$G.inv[1:ncol(x), , drop = FALSE] %*% y_fitted,
    N = N,
    X = X,
    Y = Y,
    y_scaling = y_scaling,
    coord_scaling = coord_scaling,
    coords = coords,
    formula = formula,
    covariates = x,
    nobs = nobs,
    dep_var = dep_var
  )

  class(dssp.out) <- "dsspMod"
  dssp.out
}

print.dsspMod <- function(x, ...) {
  # print model summary
  print(summary(x, ...))
}

#' Get residuals from \code{dsspMod} model
#'
#' @param object an object of class \code{dsspMod}
#' @param newdata a data frame for which to estimate residuals.
#' @param robust whether or not to use median (rather than mean) of posterior
#'   density to as estimate calculate residuals.
#' @param ... additional arguments which are ignored.
#'
#' @return vector containing residuals with same length as rows in data used.
#' @export
#'
#' @examples
#' library(sp)
#' library(gstat)
#' data(meuse.all)
#' coordinates(meuse.all) <- ~ x + y
#'
#' f <- function(x) -x ## log-prior for exponential distribution for the smoothing parameter
#'
#' ## Draw 100 samples from the posterior of eta given the data y.
#' OUTPUT <- DSSP(
#'   formula = log(zinc) ~ 1, data = meuse.all, N = 100,
#'   pars = c(0.001, 0.001), log_prior = f
#' )
#' residuals(OUTPUT)
residuals.dsspMod <- function(object, newdata, robust = TRUE, ...) {
  if (missing(newdata)) {
    y_fitted <- object$y_fitted
    y <- object$Y * object$y_scaling$scale + object$y_scaling$center
  } else {
    y_fitted <- predict.dsspMod(object, newdata = newdata)
    mf <- stats::lm(object$formula, data = newdata, method = "model.frame")
    y <- stats::model.extract(mf, "response")
  }

  metric <- ifelse(robust, stats::median, mean)

  y - apply(y_fitted, 1, metric)
}
