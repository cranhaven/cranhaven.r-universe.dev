.rc_unc.class <- setClass("rc_unc.class", representation(retcurve = "rc_est.class",
                                                         blocksize = "numeric",
                                                         nboot = "numeric",
                                                         nangles = "numeric",
                                                         alpha = "numeric",
                                                         unc = "list"))

#' An S4 class to represent the Uncertainty of the Return Curve estimates
#'
#' @slot retcurve An S4 object of class \code{rc_est.class}.
#' @slot blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @slot nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @slot nangles \loadmathjax{} Number of angles \mjeqn{m}{m} in the interval \mjeqn{(0, \pi/2)}{} \insertCite{MurphyBarltropetal2023}{ReturnCurves}. Default is \code{150} angles.
#' @slot alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% confidence intervals. Default is \code{0.05}.
#' @slot unc A list containing the median and mean estimates of the Return Curve, and the lower and upper bound of the confidence interval.
#' 
#' @keywords internal
rc_unc.class <- function(retcurve, blocksize, nboot, nangles, alpha, unc){
  .rc_unc.class(retcurve = retcurve,
                blocksize = blocksize,
                nboot = nboot,
                nangles = nangles,
                alpha = alpha,
                unc = unc)
}

#' Visualisation of the Uncertainty of the Return Curve estimates
#'
#' @description Plot method for an S4 object returned by \code{\link{rc_unc}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{rc_unc}}.
#' @param which String that indicates which return curve estimates to show. Must either be \code{"rc"} (Default), \code{"median"}, \code{"mean"} or \code{"all"}.
#' 
#' @return A ggplot object showing:
#' \item{\code{which = "rc"}}{Plots the estimated Return Curve and its uncertainty.}
#' \item{\code{which = "median"}}{Plots the median estimates of the Return Curve and its uncertainty.}
#' \item{\code{which = "mean"}}{Plots the mean estimates of the Return Curve and its uncertainty.}
#' \item{\code{which = "all"}}{Plots all the estimated Return Curve, the median and mean estimates of the Return Curve, and its uncertainty.}
#' 
#' @rdname plotrcunc
#'
#' @aliases plot,rc_unc.class
#' 
#' @keywords internal
setMethod("plot", signature = list("rc_unc.class"), function(x, which = c("rc", "median", "mean", "all")){
  X <- Y <- rcX <- rcY <- medianX <- medianY <- meanX <- meanY <- lowerX <- lowerY <- upperX <- upperY <- NULL # NULL them out to satisfy CRAN checks
  which <- match.arg(which)
  if(!which %in% c("rc", "median", "mean", "all")){
    stop("Plot type not implemented.\n 'which' should be 'rc' to plot the estimated return curve;\n 'median' to plot the median estimates of the return curve;\n 'mean' to plot the mean estimates of the return curve;\n or 'all' for all available plots.")
  }
  df <- data.frame("X" = x@retcurve@data[, 1], "Y" = x@retcurve@data[, 2])
  rcdf <- data.frame("rcX" = x@retcurve@rc[, 1], "rcY" = x@retcurve@rc[, 2])
  uncdf <- data.frame("medianX" = x@unc$median[, 1], "medianY" = x@unc$median[, 2], 
                      "meanX" = x@unc$mean[, 1], "meanY" = x@unc$mean[, 2], 
                      "lowerX" = x@unc$lower[, 1], "lowerY" = x@unc$lower[, 2], 
                      "upperX" = x@unc$upper[, 1], "upperY" = x@unc$upper[, 2])
  colours <- c("Estimated RC" = "red", "Median RC" = "orange", "Mean RC" = "brown", 
               "Lower Bound" = 1, "Upper Bound" = 1)
  if("rc" %in% which){
    rc <- ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = T, col = "grey80") +
      geom_line(data = rcdf, aes(x = rcX, y = rcY, col = names(colours)[1]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = lowerX, y = lowerY, col = names(colours)[4]), linetype = "dotted") +
      geom_line(data = uncdf, aes(x = upperX, y = upperY, col = names(colours)[5]), linetype = "dotted") +
      scale_color_manual(values = colours, 
                         guide = guide_legend(override.aes = list(linetype = c("solid", "dotted", "dotted"),
                                                                  linewidth = c(1, 0.5, 0.5)))) +
      theme_minimal() + theme(legend.title = element_blank()) +
      ggtitle(expression("Uncertainty of" ~ hat(RC)(p)))
    return(rc)
  }
  else if("median" %in% which){
    median <- ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = T, col = "grey80") +
      geom_line(data = uncdf, aes(x = medianX, y = medianY, col = names(colours)[2]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = lowerX, y = lowerY, col = names(colours)[4]), linetype = "dotted") +
      geom_line(data = uncdf, aes(x = upperX, y = upperY, col = names(colours)[5]), linetype = "dotted") +
      scale_color_manual(values = colours, 
                         guide = guide_legend(override.aes = list(linetype = c("dotted", "solid", "dotted"),
                                                                  linewidth = c(0.5, 1, 0.5)))) +
      theme_minimal() + theme(legend.title = element_blank()) +
      ggtitle(expression("Uncertainty of" ~ hat(RC)(p)))
    return(median)
  }
  else if("mean" %in% which){
    mean <- ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = T, col = "grey80") +
      geom_line(data = uncdf, aes(x = meanX, y = meanY, col = names(colours)[3]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = lowerX, y = lowerY, col = names(colours)[4]), linetype = "dotted") +
      geom_line(data = uncdf, aes(x = upperX, y = upperY, col = names(colours)[5]), linetype = "dotted") +
      scale_color_manual(values = colours, 
                         guide = guide_legend(override.aes = list(linetype = c("dotted", "solid", "dotted"),
                                                                  linewidth = c(0.5, 1, 0.5)))) +
      theme_minimal() + theme(legend.title = element_blank()) +
      ggtitle(expression("Uncertainty of" ~ hat(RC)(p)))
    return(mean)
  }
  else if("all" %in% which){
    all <- ggplot(data = df, aes(x = X, y = Y)) + geom_point(na.rm = T, col = "grey80") +
      geom_line(data = rcdf, aes(x = rcX, y = rcY, col = names(colours)[1]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = meanX, y = meanY, col = names(colours)[3]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = medianX, y = medianY, col = names(colours)[2]), linewidth = 1) +
      geom_line(data = uncdf, aes(x = lowerX, y = lowerY, col = names(colours)[4]), linetype = "dotted") +
      geom_line(data = uncdf, aes(x = upperX, y = upperY, col = names(colours)[5]), linetype = "dotted") +
      scale_color_manual(values = colours,
                         guide = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid",
                                                                               "solid", "dotted"),
                                                                  linewidth = c(1, 0.5, 1, 1, 0.5)))) +
      theme_minimal() + theme(legend.title = element_blank()) +
      ggtitle(expression("Uncertainty of" ~ hat(RC)(p)))
    return(all)
  }
})

#' Uncertainty of the Return Curve estimates
#' 
#' @name rc_unc
#' 
#' @description
#' Uncertainty assessment of the return curve estimates following the procedure of \insertCite{MurphyBarltropetal2023;textual}{ReturnCurves}.
#' 
#' @docType methods
#' 
#' @param retcurve An S4 object of class \code{rc_est.class}. See \code{\link{rc_est}} for more details.
#' @param blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @param nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @param nangles \loadmathjax{} Number of angles \mjeqn{m}{m} in the interval \mjeqn{(0, \pi/2)}{} \insertCite{MurphyBarltropetal2023}{ReturnCurves}. Default is \code{150} angles.
#' @param alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% confidence intervals. Default is \code{0.05}.
#' 
#' @return An object of S4 class \code{rc_unc.class}. This object returns the arguments of the function and an extra slot \code{unc} which is a list containing:
#' \item{median}{A vector containing the median estimates of the return curve.} 
#' \item{mean}{A vector containing the mean estimates of the return curve.} 
#' \item{lower}{A vector containing the lower bound of the confidence interval.}
#' \item{upper}{A vector containing the upper bound of the confidence interval.}
#' 
#' The \code{plot} function takes an object of S4 class \code{rc_unc.class}, and a \code{which} argument specifying the type of plot desired (see \strong{Examples}):
#' \item{\code{"rc"}}{Plots the estimated Return Curve and its uncertainty (default).}
#' \item{\code{"median"}}{Plots the median estimates of the Return Curve and its uncertainty.}
#' \item{\code{"mean"}}{Plots the mean estimates of the Return Curve and its uncertainty.}
#' \item{\code{"all"}}{Plots the estimated Return Curve, the median and mean estimates of the Return Curve together, and the associated uncertainty.}
#' 
#' @details Define a set of angles \mjdeqn{\boldsymbol{\Theta}:= \left\lbrace \frac{\pi(m+1-j)}{2(m+1)} \mid 1\leq j\leq m\right\rbrace}{} decreasing from near \mjeqn{\pi/2}{} to \mjeqn{0}{0}, 
#' and let \mjeqn{L_\theta:=\left\lbrace(x,y)\in R^2_+ | \tan(\theta)=y/x\right\rbrace}{} denote the line segment intersecting the origin with gradient \mjeqn{\tan(\theta) > 0.}{}
#' For each \mjeqn{\theta\in \boldsymbol{\Theta},}{} \mjeqn{L_\theta}{} intersects the estimated \mjeqn{\hat{RC}(p)}{} exactly once, i.e. \mjeqn{\lbrace(\hat{x}_\theta, \hat{y}_\theta)\rbrace:= \hat{RC}(p)\cap L_\theta.}{} 
#' Uncertainty of the return curve is then quantified by the distribution of \mjeqn{\hat{d}_\theta:=(\hat{x}^2_\theta + \hat{y}^2_\theta)^{1/2}}{} via a (block) bootstrap procedure. 
#' 
#' This procedure is as follows; for \mjeqn{k = 1, \ldots, }{} \code{nboot}:
#' 
#' 1. (Block) bootstrap the original data set; 
#' 
#' 2. For each \mjeqn{\theta\in \boldsymbol{\Theta},}{} obtain \mjeqn{\hat{d}_{\theta,k}}{} for the corresponding return curve point estimate.
#' 
#' Full details can be found in \insertCite{MurphyBarltropetal2023;textual}{ReturnCurves}
#' 
#' @rdname rc_uncertainty
#' 
#' @references \insertAllCited{}
#' 
#' @aliases rc_unc
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
#' rc_orig <- rc_est(margdata = margdata, p = prob, method = "hill")
#' 
#' \donttest{
#' # Set nboot = 50 for an illustrative example
#' # blocksize to account for temporal dependence
#' unc <- rc_unc(rc_orig, blocksize = 10) 
#' 
#' # Plots the estimated Return Curve 
#' plot(unc, which = "rc") 
#' 
#' # Plots the median estimates of the Return Curve
#' plot(unc, which = "median") 
#' 
#' # Plots the mean estimates of the Return Curve
#' plot(unc, which = "mean") 
#' 
#' # Plots the estimated Return Curve and its the median and mean estimates
#' plot(unc, which = "all") 
#' 
#' # To see the the S4 object's slots
#' str(unc)
#' 
#' # To access the list of vectors
#' unc@@unc
#' }
#' 
#' @export
#' 
rc_unc <- function(retcurve, blocksize = 1, nboot = 250, nangles = 150, alpha = 0.05){ 
  if(!inherits(retcurve, "rc_est.class")){
    stop("The retcurve argument needs to be an object of class rc_est.class.")
  }
  if(nboot < 1 | nboot %% 1 != 0){
    stop("The number of bootstrap samples needs to be a positive integer.")
  }
  if(nangles < 1 | nangles %% 1 != 0){
    stop("The number of angles needs to be a positive integer.")
  }
  if(alpha < 0 | alpha > 1){
    stop("The significance level needs to be in [0, 1].")
  }
  if(alpha > 0.5){
    warning("This will lead to a confidence interval smaller than 50%. Perhaps you mean 1-alpha.")
  }
  constrainedshape = retcurve@constrainedshape
  result <- rc_unc.class(retcurve = retcurve, blocksize = blocksize, nboot = nboot, nangles = nangles, 
                         alpha = alpha, unc = list())
  rc_origin <- result@retcurve@rc
  data <- result@retcurve@data
  w <- result@retcurve@w
  qmarg <- result@retcurve@qmarg
  p <- result@retcurve@p
  method <- result@retcurve@method
  q <- result@retcurve@q
  qalphas <- result@retcurve@qalphas
  k <- result@retcurve@k
  constrained <- result@retcurve@constrained
  tol <- result@retcurve@tol
  par_init <- result@retcurve@par_init
  n <- dim(data)[1]
  angles <- ((nangles:1)/(nangles + 1)) * (pi/2)
  grad <- tan(angles)
  data0 <- apply(data, 2, min)
  norms <- lapply(1:nangles, function(i) vector())
  bootwarnmargconst <- integer()
  bootwarnmargunconst <- integer()
  for(i in 1:nboot){
    bootdata <- block_bootstrap_function(data = data, k = blocksize, n = n)
    margdataboot <- withCallingHandlers(margtransf(data = bootdata, qmarg = qmarg, constrainedshape = constrainedshape),
                                        warning = function(war){
                                          if(war$message == "MLE for the constrained shape parameter of the GPD is close to -1. \n Unconstrained MLE is likely to be < -1."){
                                            bootwarnmargconst <<- c(bootwarnmargconst, i)
                                          }
                                          else if(war$message == "MLE for the shape parameter of the GPD is < -1. \n Fitted endpoint is the maximum data point."){
                                            bootwarnmargunconst <<- c(bootwarnmargunconst, i)
                                          }
                                          invokeRestart("muffleWarning")})
    rc_data <- tryCatch(rc_est(margdata = margdataboot, w = w, p = p, method = method, q = q, qalphas = qalphas, k = k, constrained = constrained, tol = tol, par_init = par_init),
                        error = function(e){
                          message("Optimisation issues due to infinite values. \n Try setting constrainedshape = TRUE when transforming the data to exponential.")
                          stop(invisible(e))
                        })
    rc_orig <- rc_data@rc
    rc_orig <- rbind(c(data0[1], rc_orig[1, 2]), rc_orig, c(rc_orig[dim(rc_orig)[1], 1], data0[2]))
    curve_w <- atan((rc_orig[, 2] - data0[2])/(rc_orig[, 1] - data0[1]))
    for(j in 1:nangles){
      idx <- min(which(angles[j] >= curve_w))
      data1 <- rc_orig[idx, ] - data0
      data2 <- rc_orig[idx - 1, ] - data0
      s <- (data1[1] * tan(angles[j]) - data1[2])/((data2[2] - data1[2]) - (data2[1] - data1[1]) * tan(angles[j]))
      xhat <- data1[1] + s*(data2[1] - data1[1])
      yhat <- data1[2] + s*(data2[2] - data1[2])
      norms[[j]][i] <- sqrt(xhat^2 + yhat^2)
    }
  }
  if(length(bootwarnmargconst) > 0){
    warning(sprintf("In iterations %s of the bootstrap procedure: estimated constrained GPD shape parameter is close to -1. \n Unconstrained MLE is likely to be < -1.",
                    paste(bootwarnmargconst, collapse = ", ")))
  }
  if(length(bootwarnmargunconst) > 0){
    warning(sprintf("In iterations %s of the bootstrap procedure: estimated GPD shape parameter is < -1. \n Fitted endpoint is the maximum data point.",
                    paste(bootwarnmargunconst, collapse = ", ")))
  }
  lb <- sapply(1:nangles, function(i) quantile(norms[[i]], alpha/2))
  ub <- sapply(1:nangles, function(i) quantile(norms[[i]], 1 - alpha/2))
  med <- sapply(1:nangles, function(i) quantile(norms[[i]], 0.5))
  mea <- sapply(1:nangles, function(i) mean(norms[[i]]))
  rc_mean <- cbind(mea/sqrt(1 + grad^2) + data0[1], grad * (mea/sqrt(1 + grad^2)) + data0[2])
  rc_median <- cbind(med/sqrt(1 + grad^2) + data0[1], grad * (med/sqrt(1 + grad^2)) + data0[2])
  rc_lb <- cbind(lb/sqrt(1 + grad^2) + data0[1], grad * (lb/sqrt(1 + grad^2)) + data0[2])
  rc_ub <- cbind(ub/sqrt(1 + grad^2) + data0[1], grad * (ub/sqrt(1 + grad^2)) + data0[2])
  result@unc <- list("median" = rc_median, "mean" = rc_mean, "lower" = rc_lb, "upper" = rc_ub)
  return(result)
}



