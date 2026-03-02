ranktransform <- function(data, thresh) rank(data)[data <= thresh]/(length(data) + 1) 
gpdtransform <- function(data, thresh, par, qmarg) 1 - (1 - qmarg)*pgpd(data, loc = thresh, scale = par[1], shape = par[2], lower.tail = F)

gpdlikelihood <- function(data, par){
  sigma <- par[1]
  xi <- par[2]
  gpdpdf <- function(x, sigma, xi){
    if(abs(xi) < 1e-10){
      dens <- (1/sigma) * exp(-x/sigma)
      return(dens)
    }
    else{
      dens <- (1/sigma) * ((1 + (xi/sigma) * x)^(-1/xi - 1))
      return(dens)
    }
  }
  if(sigma <= 0 | xi <= -1){
    return(1000e10)
  }
  else if(all(1 + (xi/sigma) * data > 0)){
    logdens <- log(gpdpdf(x = data, sigma = sigma, xi = xi))
    return(-sum(logdens))
  }
  else{
    return(1000e10)
  }
}

empirical_cdf <- function(data, qmarg, constrainedshape) { 
  compldata <- data[complete.cases(data)]
  u <- c()
  thresh <- quantile(compldata, qmarg)
  if (qmarg == 1) {
    stop("Marginal quantile leading to threshold u too high and no exceedances to fit the GPD.")
  }
  if(constrainedshape == T){
    excdata <- compldata[compldata > thresh] - thresh
    opt <- optim(par = c(0.1, 0.1), fn = gpdlikelihood, data = excdata)
    par <- opt$par
    if(par[2] < -0.999){
      warning("MLE for the constrained shape parameter of the GPD is close to -1. \n Unconstrained MLE is likely to be < -1.",
              call. = FALSE)
    }
  }
  else if(constrainedshape == F){
    par <- suppressWarnings(gpd.fit(compldata, threshold = thresh, show = FALSE)$mle)
    if(par[2] <= -1){
      warning("MLE for the shape parameter of the GPD is < -1. \n Fitted endpoint is the maximum data point.",
              call. = FALSE)
    }
  }
  if(par[2] < -0.5 && par[2] > -1){
    warning("MLE for the shape parameter of the GPD is in (-1, -0.5). \n Non-regular MLE and a very short marginal tail is estimated.",
            call. = FALSE)
  }
  u[!is.na(data) & data <= thresh] <- ranktransform(data = compldata, thresh = thresh)
  u[!is.na(data) & data > thresh] <- gpdtransform(data = compldata[compldata > thresh], thresh = thresh, par = par, qmarg = qmarg)
  u[is.na(data)] <- NA
  return(list("parameters" = par, "thresh" = thresh,"data" = u))
}

.margtransf.class <- setClass("margtransf.class", representation(data = "array",
                                                                 qmarg = "numeric",
                                                                 constrainedshape = "logical",
                                                                 parameters = "array",
                                                                 thresh = "numeric",
                                                                 dataexp = "array"))

#' An S4 class to represent the Marginal Transformation
#'
#' @slot data A matrix containing the data on the original margins.
#' @slot qmarg A vector containing the marginal quantile used to fit the Generalised Pareto Distribution (GPD) for each variable. Default is \code{rep(0.95, 2)}.
#' @slot constrainedshape Logical. If \code{TRUE} (Default), the estimated shape parameter of the Generalised Pareto Distribution (GPD) is constrained to strictly above \code{-1}.
#' @slot parameters A vector containing the scale and shape parameters of the Generalised Pareto Distribution (GPD).
#' @slot thresh \loadmathjax{} A vector containing the threshold \mjeqn{u}{u} above which the Generalised Pareto Distribution (GPD) is fitted.
#' @slot dataexp A matrix containing the data on standard exponential margins.
#' 
#' @keywords internal
margtransf.class <- function(data, qmarg, constrainedshape, parameters, thresh, dataexp){
  .margtransf.class(data = data,
                    qmarg = qmarg,
                    constrainedshape = constrainedshape,
                    parameters = parameters,
                    thresh = thresh,
                    dataexp = dataexp)
}

#' Visualisation of the Marginal Transformation
#'
#' @description Plot method for an S4 object returned by \code{\link{margtransf}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{margtransf}}.
#' @param which String that indicates which type of plot to show. Must either be \code{"all"} (Default), \code{"hist"}, \code{"ts"} or \code{"joint"}.
#' 
#' @return A ggplot object showing:
#' \item{\code{which = "hist"}}{histograms of each variable on original and standard exponential margins.}
#' \item{\code{which = "ts"}}{time series of each variable on original and standard exponential margins.}
#' \item{\code{which = "joint"}}{joint distribution on original and standard exponential margins.}
#' \item{\code{which = "all"}}{all the available plots.}
#'
#' @rdname plotmargtransf
#'
#' @aliases plot,margtransf.class
#' 
#' @keywords internal
setMethod("plot", signature = list("margtransf.class"), function(x, which = c("all", "hist", "ts", "joint")){
  X <- Y <- Xexp <- Yexp <- NULL # NULL them out to satisfy CRAN checks
  which <- match.arg(which)
  if(!which %in% c("all", "hist", "ts", "joint")){
    stop("Plot type not implemented.\n 'which' should be 'hist' to plot the histograms of the data;\n 'ts' to plot the time series of each variable;\n 'joint' to plot the joint distribution;\n or 'all' for all available plots.")
  }
  df <- data.frame("X" = x@data[, 1], "Y" = x@data[, 2], "Xexp" = x@dataexp[, 1], "Yexp" = x@dataexp[, 2])
  plots <- list()
  if ("all" %in% which || "hist" %in% which) {
    origX <- ggplot(data = df, aes(x = X)) + geom_histogram(aes(y = after_stat(density)), col = "darkred", fill = "red", alpha = 0.3, 
                                                        na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
      theme_minimal() + labs(x = "X", y = "Frequency") + ggtitle("Original margin of X")
    expX <- ggplot(data = df, aes(x = Xexp)) + geom_histogram(aes(y = after_stat(density)), col = "darkred", fill = "red", alpha = 0.3, 
                                                          na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
      theme_minimal() + labs(x = expression(X[exp]), y = "Frequency") + ggtitle("Marginal transformation of X")
    origY <- ggplot(data = df, aes(x = Y)) + geom_histogram(aes(y = after_stat(density)), col = "darkblue", fill = "blue", alpha = 0.3, 
                                                        na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
      theme_minimal() + labs(x = "Y", y = "Frequency") + ggtitle("Original margin of Y")
    expY <- ggplot(data = df, aes(x = Yexp)) + geom_histogram(aes(y = after_stat(density)), col = "darkblue", fill = "blue", alpha = 0.3, 
                                                          na.rm = TRUE, show.legend = FALSE, bins = 30, boundary = 0) +
      theme_minimal() + labs(x = expression(Y[exp]), y = "Frequency") + ggtitle("Marginal transformation of Y")
    plots <- c(plots, list(origX, expX, origY, expY))
  }
  if ("all" %in% which || "ts" %in% which) {
    tsorigX <- ggplot(data = df, aes(x = 1:length(X), y = X)) + geom_line(na.rm = T) +
      theme_minimal() + labs(x = "Index", y = "X") + 
      ggtitle("Time series of X")
    tsexpX <- ggplot(data = df, aes(x = 1:length(Xexp), y = Xexp)) + geom_line(na.rm = T) +
      theme_minimal() + labs(x = "Index", y = expression(X[exp])) + 
      ggtitle(expression("Times series of" ~ X[exp]))
    tsorigY <- ggplot(data = df, aes(x = 1:length(Y), y = Y)) + geom_line(na.rm = T) +
      theme_minimal() + labs(x = "Index", y = "Y") + 
      ggtitle("Time series of Y")
    tsexpY <- ggplot(data = df, aes(x = 1:length(Yexp), y = Yexp)) + geom_line(na.rm = T) +
      theme_minimal() + labs(x = "Index", y = expression(Y[exp])) + 
      ggtitle(expression("Times series of" ~ Y[exp]))
    plots <- c(plots, list(tsorigX, tsexpX, tsorigY, tsexpY))
  }
  if ("all" %in% which || "joint" %in% which) {
    origjoint <- ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = TRUE) +
      theme_minimal() +
      ggtitle("Original margins")
    expjoint <- ggplot(data = df, aes(x = Xexp, y = Yexp)) + geom_point(na.rm = TRUE) +
      theme_minimal() + labs(x = expression(X[exp]), y = expression(Y[exp])) +
      ggtitle("Standard exponential margins")
    plots <- c(plots, list(origjoint, expjoint))
  }
  grid.arrange(grobs = plots, ncol = 2)
})

#' Marginal Transformation
#' 
#' @name margtransf
#' 
#' @description
#' Marginal transformation of a  bivariate random vector to standard exponential margins following \insertCite{ColesTawn1991;textual}{ReturnCurves}. Variables within each margin are assumed identically distributed.
#' 
#' @docType methods
#' 
#' @param data A matrix containing the data on the original margins.
#' @param qmarg A vector containing the marginal quantile used to fit the Generalised Pareto Distribution (GPD) for each variable. Default is \code{rep(0.95, 2)}.
#' @param constrainedshape Logical. If \code{TRUE} (Default), the estimated shape parameter of the Generalised Pareto Distribution (GPD) is constrained to lie strictly above \code{-1}.
#' 
#' @return An object of S4 class \code{margtransf.class}. This object returns the arguments of the function, a slot \code{parameters} containing a matrix with the shape and scale parameters of the Generalised Pareto Distribution (GPD) for each variable, a slot \code{thresh} containing a vector with the threshold \mjeqn{u}{u} above which the GPD is fitted, and a slot \code{dataexp} containing a matrix with the data on standard exponential margins. 
#' 
#' The \code{plot} function takes an object of S4 class \code{margtransf.class}, and a \code{which} argument specifying the type of plot desired (see \strong{Examples}):
#' \item{\code{"hist"}}{Plots the marginal distributions of the two variables on original and standard exponential margins.}
#' \item{\code{"ts"}}{Plots the time series of the two variables on original and standard exponential margins.}
#' \item{\code{"joint"}}{Plots the joint distribution of the two variables on original and standard exponential margins.}
#' \item{\code{"all"}}{Plots all the above mentioned plots (default).}
#' 
#' @details \loadmathjax{} Given a threshold value \mjeqn{u}{u}, each stationary random vector 
#' is transformed by using the empirical cumulative distribution function 
#' (cdf) below \mjeqn{u}{u}, and a Generalise Pareto Distribution (GPD) fit above \mjeqn{u}{u}.    
#' 
#' The option to constrain \mjeqn{\xi > -1}{} is included as \mjeqn{\xi \leq -1}{} implies that the fitted
#' upper endpoint of the distribution's support is the maximum data point. This situation is rarely encountered in practice.
#' 
#' @rdname marginaltransformation
#' 
#' @references \insertAllCited{}
#' 
#' @aliases margtransf
#' 
#' @examples
#' library(ReturnCurves)
#' 
#' data(airdata)
#' 
#' n <- dim(airdata)[1]
#' 
#' margdata <- margtransf(airdata)
#' 
#' # Plots the marginal distributions of X and Y on original vs standard exponential margins
#' plot(margdata, which = "hist") 
#' 
#' # Plots the time series of X and Y on original vs standard exponential margins
#' plot(margdata, which = "ts") 
#' 
#' # Plots the joint distribution of X and Y on original vs standard exponential margins
#' plot(margdata, which = "joint") 
#' 
#' # Plots all the available plots
#' plot(margdata, which = "all") 
#' 
#' # To see the the S4 object's slots
#' str(margdata)
#' 
#' # To access the matrix with the data on standard exponential margins
#' margdata@@dataexp
#' 
#' @export
#' 
margtransf <- function(data, qmarg = rep(0.95, 2), constrainedshape = TRUE){
  data <- as.matrix(data)
  if(is.null(dim(data)) || dim(data)[2] > 2){
    warning("Estimation of the Return Curves and/or ADF are only implemented for a bivariate setting.")
  }
  if(dim(data)[1] < 500){
    warning("Sample size less than 500 data points. Small sample size is subject to higher variability and subsequent errors in modelling.")
  }
  if(any(qmarg < 0) | any(qmarg > 1)){
    stop("Marginal quantile needs to be in [0, 1].")
  }
  result <- margtransf.class(data = data, qmarg = qmarg, constrainedshape = constrainedshape, parameters = array(), thresh = numeric(), dataexp = array())
  nas <- colSums(is.na(data))
  if(any(nas > 0)){
    indnas <- which(nas > 0)
    for(i in indnas){
      warning(paste0("There are ", nas[i], " missing values in margin X", i, ".\n These were removed."))
    }
  }
  dataunif <- matrix(NA, ncol = 2, nrow = dim(data)[1])
  par <- matrix(NA, ncol = 2, nrow = 2)
  thresh <- c()
  for(i in 1:2){
    marg <- empirical_cdf(data[, i], qmarg = qmarg[i], constrainedshape = constrainedshape)
    par[, i] <- marg$parameters
    thresh[i] <- marg$thresh
    dataunif[, i] <- marg$data
  }
  result@parameters <- par
  result@thresh <- thresh
  result@dataexp <- apply(dataunif, 2, qexp)
  return(result)
}  




