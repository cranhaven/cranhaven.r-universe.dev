.rc_gof.class <- setClass("rc_gof.class", representation(retcurve = "rc_est.class",
                                                         blocksize = "numeric",
                                                         nboot = "numeric",
                                                         alpha = "numeric",
                                                         gof = "list"))


#' An S4 class to represent the Goodness-of-Fit of the Return Curve estimates
#'
#' @slot retcurve An S4 object of class \code{rc_est.class}.
#' @slot blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @slot nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @slot nangles \loadmathjax{} Number of angles \mjeqn{m}{m} in the interval \mjeqn{(0, \pi/2)}{} \insertCite{MurphyBarltropetal2023}{ReturnCurves}. Default is \code{150} angles.
#' @slot alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% confidence intervals. Default is \code{0.05}.
#' @slot gof A list containing the median of the empirical probability and the lower and upper bound of the confidence interval.
#' 
#' @keywords internal
rc_gof.class <- function(retcurve, blocksize, nboot, alpha, gof){
  .rc_gof.class(retcurve = retcurve,
                blocksize = blocksize,
                nboot = nboot,
                alpha = alpha,
                gof = gof)
}

#' Visualisation of the Goodness-of-Fit of the Return Curve estimates
#'
#' @description Plot method for an S4 object returned by \code{\link{rc_gof}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{rc_gof}}.
#' 
#' @return A ggplot object comparing the true and median estimates of the empirical probability of lying in a survival region.
#'
#' @rdname plotrcgof
#'
#' @aliases plot,rc_gof.class
#' 
#' @keywords internal
setMethod("plot", signature = list("rc_gof.class"), function(x){
  angles <- pX <- pY <- prob <- upper <- lower <- NULL # NULL them out to satisfy CRAN checks
  df <- data.frame("angles" = 1:length(x@gof$median), x@gof, "pX" = c(rev(1:length(x@gof$median)), 1:length(x@gof$median)),
                   "pY" = c(rev(x@gof$lower), x@gof$upper), "prob" = rep(x@retcurve@p, length(x@gof$median)))
  coloursl <- c("Confidence interval" = 1, "Median estimate" = 1, "True probability" = 2)
  ggplot(data = df, aes(x = pX, y = pY)) + geom_polygon(fill = "grey80", col = NA) +
    geom_line(aes(x = angles, y = median, col = names(coloursl)[2])) +
    geom_line(aes(x = angles, y = upper, col = names(coloursl)[1]), linetype = "dashed") +
    geom_line(aes(x = angles, y = lower, col = names(coloursl)[1]), linetype = "dashed") + 
    geom_line(aes(x = angles, y = prob, col = names(coloursl)[3])) +
    labs(x = "Angle Index", y = "Probability") +
    scale_color_manual(values = coloursl,
                       guide = guide_legend(override.aes = list(linetype = c("dashed", "solid", "solid"))))  +
    # ylim(c(range(df$lower)[1] - 2.220446e-16, range(df$upper)[2] + 2.220446e-16)) + 
    ylim(min(df$lower), max(df$upper)) + 
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ggtitle(expression("Goodness of fit of" ~ hat(RC)(p)))
  
})

#' Goodness of fit of the Return Curve estimates
#' 
#' @name rc_gof
#' 
#' @description
#' Assessment of the goodness-of-fit of the return curve estimates following the approach of \insertCite{MurphyBarltropetal2023;textual}{ReturnCurves}.
#' 
#' @docType methods
#' 
#' @inheritParams rc_unc
#'  
#' @return An object of S4 class \code{rc_gof.class}. This object returns the arguments of the function and an extra slot \code{gof} which is a list containing:
#' \item{median}{A vector containing the median of the empirical probability of lying in a survival region.} 
#' \item{lower}{A vector containing the lower bound of the confidence interval.}
#' \item{upper}{A vector containing the upper bound of the confidence interval.}
#' 
#' @details Given a return curve RC(\mjeqn{p}{p}), the probability of lying in a survival region is \mjeqn{p}{p}. 
#' Let \mjdeqn{\boldsymbol{\Theta}:= \left\lbrace \frac{\pi(m+1-j)}{2(m+1)} \mid 1\leq j\leq m\right\rbrace}{} be a set of angles decreasing from near \mjeqn{\pi/2}{} to \mjeqn{0}{0}.
#' For each angle \mjeqn{\theta_j\in \boldsymbol{\Theta,}}{} and corresponding point in the estimated return curve \mjeqn{\lbrace (\hat{x}_{\theta_j}, \hat{y}_{\theta_j}) \rbrace}{}, 
#' the empirical probability \mjeqn{\hat{p}_j}{p} of lying in the survival region is given by the proportion of points in the region
#' \mjeqn{(\hat{x}_{\theta_j}, \infty) \times (\hat{y}_{\theta_j}, \infty)}{}.
#' 
#' Thus, for each angle \mjeqn{\theta_j\in \boldsymbol{\Theta,}}{} a (block) bootstrap procedure to the original data set is applied, and
#' the empirical probabilities \mjeqn{\hat{p}_j}{} estimated. Then, the median and \mjeqn{(1-\alpha)}{}\% pointwise confidence intervals are obtained for each \mjeqn{\theta_j}{}.
#' Function \code{plot} shows the median of \mjeqn{\hat{p}_j}{}, the confidence intervals and the true probability \mjeqn{p}{p}; ideally, this value should be contained in the confidence region.
#' 
#' We note that due to the use of empirical probabilities, the value of \mjeqn{p}{p} should be within the range of the data and not too extreme.
#' 
#' @rdname rc_gof
#' 
#' @references \insertAllCited{}
#' 
#' @aliases rc_gof
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
#' # blocksize to account for temporal dependence
#' gof <- rc_gof(retcurve = rc_orig, blocksize = 10)
#' 
#' plot(gof)
#' 
#' # To see the the S4 object's slots
#' str(gof)
#' 
#' # To access the list of vectors
#' gof@@gof
#' 
#' @export
#'  
rc_gof <- function(retcurve, blocksize = 1, nboot = 250, nangles = 150, alpha = 0.05){ 
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
  result <- rc_gof.class(retcurve = retcurve, blocksize = blocksize, nboot = nboot, alpha = alpha, gof = list())
  rc_origin <- result@retcurve@rc
  data <- result@retcurve@data
  w <- result@retcurve@w
  n <- dim(data)[1]
  angles <- ((nangles:1)/(nangles + 1)) * (pi/2)
  grad <- tan(angles)
  data0 <- apply(data, 2, min)
  emp_prob <- lapply(1:nangles, function(i) vector())
  curve_w <- atan((rc_origin[, 2] - data0[2])/(rc_origin[, 1] - data0[1]))
  angles_est <- matrix(NA, ncol = dim(data)[2], nrow = nangles)
  for(i in 1:nangles){
    idx <- min(which(angles[i] >= curve_w))
    data1 <- rc_origin[idx, ] - data0
    data2 <- rc_origin[idx - 1, ] - data0
    s <- (data1[1] * tan(angles[i]) - data1[2])/((data2[2] - data1[2]) - (data2[1] - data1[1]) * tan(angles[i]))
    xhat <- data1[1] + s*(data2[1] - data1[1]) + data0[1]
    yhat <- data1[2] + s*(data2[2] - data1[2]) + data0[2]
    angles_est[i, ] <- c(xhat, yhat)
  }
  for(i in 1:nboot){
    bootdata <- block_bootstrap_function(data = data, k = blocksize, n = n)
    for(j in 1:nangles){
      emp_prob[[j]][i] <- mean(bootdata[, 1] > angles_est[j, 1] & bootdata[, 2] > angles_est[j, 2])
    }
  }
  lb <- sapply(1:nangles, function(i) quantile(emp_prob[[i]], alpha/2))
  ub <- sapply(1:nangles, function(i) quantile(emp_prob[[i]], 1 - alpha/2))
  med <- sapply(1:nangles, function(i) quantile(emp_prob[[i]], 0.5))
  result@gof <- list("median" = med, "lower" = lb, "upper" = ub)
  return(result)
}




