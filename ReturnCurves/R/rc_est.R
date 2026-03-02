curve_inverse_transform <- function(curveunif, data, par, thresh, qmarg){
  nvec <- c()
  nvec[curveunif > qmarg] <- qgpd((curveunif[curveunif > qmarg] - qmarg)/(1 - qmarg), loc = thresh, scale = par[1], shape = par[2])
  nvec[curveunif <= qmarg] <- quantile(data, curveunif[curveunif <= qmarg])
  return(nvec)
}

.rc_est.class <- setClass("rc_est.class", representation(data = "array",
                                                         qmarg = "numeric",
                                                         constrainedshape = "logical",
                                                         w = "numeric",
                                                         p = "numeric",
                                                         method = "character",
                                                         q = "numeric",
                                                         qalphas = "numeric",
                                                         k = "numeric",
                                                         constrained = "logical",
                                                         tol = "numeric",
                                                         par_init = "numeric",
                                                         interval = "numeric",
                                                         rc = "array"))

#' An S4 class to represent the estimation of the Return Curve
#'
#' @slot data A matrix containing the data on the original margins.
#' @slot qmarg A vector containing the marginal quantile used to fit the Generalised Pareto Distribution (GPD) for each variable. Default is \code{rep(0.95, 2)}.
#' @slot constrainedshape Logical. If \code{TRUE} (Default), the estimated shape parameter of the Generalised Pareto Distribution (GPD) is constrained to strictly above \code{-1}.
#' @slot w Sequence of rays between \code{0} and \code{1}. Default is \code{seq(0, 1, by = 0.01)}.
#' @slot method String that indicates which method is used for the estimation of the angular dependence function. Must either be \code{"hill"}, to use the Hill estimator \insertCite{Hill1975}{ReturnCurves}, or \code{"cl"} to use the smooth estimator based on Bernstein-Bezier polynomials estimated by composite maximum likelihood.
#' @slot p \loadmathjax{} Curve survival probability. Must be \mjeqn{p < 1-q}{p < 1-q} and \mjeqn{p < 1-q_\alpha}{p < 1-qalphas}.
#' @slot q Marginal quantile used for the min-projection variable \mjeqn{T^1}{} at angle \mjeqn{\omega}{} \mjeqn{\left(t^1_\omega = t_\omega - u_\omega | t_\omega > u_\omega\right)}{}, and/or Hill estimator \insertCite{Hill1975}{ReturnCurves}. Default is \code{0.95}.
#' @slot qalphas A vector containing the marginal quantile used for the Heffernan and Tawn conditional extremes model \insertCite{HeffernanTawn2004}{ReturnCurves} for each variable, if \code{constrained = TRUE}. Default set to \code{rep(0.95, 2)}.
#' @slot k Polynomial degree for the Bernstein-Bezier polynomials used for the estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default set to \code{7}.
#' @slot constrained Logical. If \code{FALSE} (default) no knowledge of the conditional extremes parameters is incorporated in the angular dependence function estimation. 
#' @slot tol Convergence tolerance for the composite maximum likelihood procedure. Default set to \code{0.0001}.
#' @slot par_init Initial values for the parameters \mjeqn{\beta}{} of the Bernstein-Bezier polynomials used for estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default set to a vector of \code{0} of length \code{k-1}.
#' @slot interval Maximum likelihood estimates \mjeqn{\hat{\alpha}^1_{x\mid y}}{} and \mjeqn{\hat{\alpha}^1_{y\mid x}}{} from the conditional extremes model if \code{constrained = TRUE}.
#' @slot rc A matrix containing the estimates of the Return Curve.
#' 
#' @keywords internal
rc_est.class <- function(data, qmarg, constrainedshape, w, p, method, q, qalphas, k, constrained, tol, par_init, interval, rc){
  .rc_est.class(data = data,
                qmarg = qmarg,
                constrainedshape = constrainedshape,
                w = w,
                p = p,
                method = method,
                q = q,
                qalphas = qalphas,
                k = k,
                constrained = constrained,
                tol = tol,
                par_init = par_init,
                interval = interval,
                rc = rc)
}

#' Visualisation of the Return Curve estimates
#'
#' @description Plot method for an S4 object returned by \code{\link{rc_est}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{rc_est}}.
#' 
#' @return A ggplot object showing the original data with the estimated Return Curve.
#'
#' @rdname plotrcest
#'
#' @aliases plot,rc_est.class
#' 
#' @keywords internal
setMethod("plot", signature = list("rc_est.class"), function(x){
  X <- Y <- rcX <- rcY <- NULL # NULL them out to satisfy CRAN checks
  df <- data.frame("X" = x@data[, 1], "Y" = x@data[, 2])
  rcdf <- data.frame("rcX" = x@rc[, 1], "rcY" = x@rc[, 2])
  ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = T) +
    geom_line(data = rcdf, aes(x = rcX, y = rcY), col = "red", linewidth = 1) +
    theme_minimal() +
    ggtitle(expression("Estimation of" ~ hat(RC)(p)))
})

#' Estimation of the Return Curve
#' 
#' @name rc_est
#' 
#' @description
#' \loadmathjax{} Estimation of the \mjeqn{p}{p}-probability return curve following \insertCite{MurphyBarltropetal2023;textual}{ReturnCurves}.
#'  
#' @docType methods
#' 
#' @param p Curve survival probability. Must be \mjeqn{p < 1-q}{p < 1-q} and \mjeqn{p < 1-q_\alpha}{p < 1-qalphas}.
#' @inheritParams adf_est
#' 
#' @return An object of S4 class \code{rc_est.class}. This object returns the arguments of the function and extra slot \code{rc} 
#' \item{\code{interval}:}{A vector containing the maximum likelihood estimates from the conditional extremes model, \mjeqn{\hat{\alpha}^1_{x\mid y}}{} and \mjeqn{\hat{\alpha}^1_{y\mid x}}{}, if \code{constrained = TRUE}. If \code{constrained = FALSE}, then \code{c(0, 1)} is returned; we note that this has no meaningful interpretation as the estimation is performed in an unconstrained interval.}
#' \item{\code{rc}:}{A matrix with the estimates of the Return Curve.}
#' 
#' @details Given a probability \mjeqn{p}{p} and a joint survival function \mjeqn{Pr(X>x, Y>y)}{}, 
#' the \mjeqn{p}{p}-probability return curve is defined as 
#' \mjdeqn{RC(p):=\left\lbrace(x, y) \in R^2: Pr(X>x, Y>y)=p\right\rbrace.}{} 
#' 
#' This method focuses on estimation of \mjeqn{RC(p)}{RC(p)} for small \mjeqn{p}{p} near \mjeqn{0}{0}, so that \mjeqn{(X,Y)}{} are in the tail of the distribution.
#' 
#' \mjeqn{Pr(X>x, Y>y)}{} is estimated using the angular dependence function \mjeqn{\lambda(\omega)}{} introduced by \insertCite{WadsworthTawn2013;textual}{ReturnCurves}. More details on how to estimate \mjeqn{\lambda(\omega)}{} can be found in \code{\link{adf_est}}.
#' 
#' The return curve estimation \mjeqn{\hat{RC}(p)}{} is done on standard exponential margins and then back transformed onto the original margins.
#' 
#' @rdname returncurve
#' 
#' @references \insertAllCited{}
#' 
#' @aliases rc_est
#' 
#' @examples
#' library(ReturnCurves)
#' 
#' data(airdata)
#' 
#' n <- dim(airdata)[1]
#' 
#' prob <- 10/n
#' 
#' margdata <- margtransf(airdata)
#' 
#' retcurve <- rc_est(margdata = margdata, p = prob, method = "hill")
#' 
#' plot(retcurve)
#' 
#' # To see the the S4 object's slots
#' str(retcurve)
#' 
#' # To access the return curve estimation
#' retcurve@@rc
#' 
#' # If constrained = T, the MLE estimates for the conditional extremes model
#' # can be accessed as
#' retcurve@@interval
#' 
#' @export
rc_est <- function(margdata, w = NULL, p, method = c("hill", "cl"), q = 0.95, qalphas = rep(0.95, 2), k = 7, constrained = FALSE, tol = 0.001, par_init = rep(0, k - 1)){
  if(!inherits(margdata, "margtransf.class")){
    stop("The margdata argument needs to be an object of class margtransf.class.")
  }
  data <- margdata@data
  qmarg <- margdata@qmarg
  constrainedshape <- margdata@constrainedshape
  parameters <- margdata@parameters
  thresh <- margdata@thresh
  dataexp <- margdata@dataexp
  if(is.null(dim(data)) || dim(data)[2] > 2){
    stop("Estimation of the Return Curve is only implemented for a bivariate setting.")
  }
  if(any(qmarg < 0) | any(qmarg > 1)){
    stop("Marginal quantiles need to be in [0, 1].")
  }
  if(is.null(w)){
    if(method == "cl"){
      w <- seq(0, 1, by = 0.01)
    }
    else if(method == "hill"){
      w <- seq(0, 1, by = 0.001)
    }
    else{
      stop("Method to estimate the ADF not implemented.")
    }
  }
  if(any(w < 0) | any(w > 1)){
    stop("Rays need to be in [0, 1].")
  }
  if(!method %in% c("hill", "cl")){
    stop("ADF needs to be estimated either through the Hill estimator or Composite likelihood estimator.")
  }
  if(p < 0 | p > 1){
    stop("Probability needs to be in [0, 1].")
  }
  if(p > 1 - qmarg[1] | p > 1 - qmarg[2] | p > 1 - q | p > 1 - qalphas[1] | p > 1 - qalphas[2]){
    warning("The curve survival probability p should not be too extreme and within the range of the data, i.e. smaller than the marginal quantiles.")
  }
  if(q < max(qmarg) | any(qalphas < max(qmarg))){
    stop("Marginal quantiles need to be higher than the highest marginal quantile used for the marginal transformation.")
  }
  result <- rc_est.class(data = data, qmarg = qmarg, constrainedshape = constrainedshape, w = w, p = p, method = method, q = q, qalphas = qalphas, k = k, constrained = constrained, tol = tol, par_init = par_init, interval = double(), rc = array())
  rc_expdata <- rc_exp(margdata = margdata, w = w, p = p, method = method, q_minproj = q, qalphas = qalphas, k = k, constrained = constrained, tol = tol, par_init = par_init)
  rc_data <- rc_expdata$"rc"
  curveunif <- apply(rc_data, 2, pexp)
  data <- data[complete.cases(data), ]
  result@interval <- rc_expdata$"alphas"
  result@data <- data
  result@rc <- sapply(1:dim(curveunif)[2], function(i) curve_inverse_transform(curveunif[, i], data = data[, i], par = parameters[, i], thresh = thresh[i], qmarg = qmarg[i]))
  return(result)
}




