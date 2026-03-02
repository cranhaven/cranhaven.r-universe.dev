gpdunc <- function(data, par, thresh, blocksize, nboot, alpha){
  excdata <- data[data > thresh]
  nexc <- length(excdata)
  empquantile <- sort(excdata)
  modquantile <- qgpd((1:nexc) / (nexc + 1), loc = thresh, scale = par[1], shape = par[2])
  empquantileboot <- matrix(NA, nrow = nboot, ncol = nexc)
  for(i in 1:nboot){
    bdata <- block_bootstrap_function(data = excdata, k = blocksize, n = nexc)
    empquantileboot[i, ] <- sort(bdata)
  }
  ub <- apply(empquantileboot, 2, quantile, probs = 1 - alpha/2)
  lb <- apply(empquantileboot, 2, quantile, probs = alpha/2)
  return(list("model" = modquantile, "empirical" = empquantile, "lower" = lb, "upper" = ub))
}

.marggpd.class <- setClass("marggpd.class", representation(margdata = "margtransf.class",
                                                           blocksize = "numeric",
                                                           nboot = "numeric",
                                                           alpha = "numeric",
                                                           marggpd = "list"))

#' An S4 class to represent the assessment of the the Marginal Tail Fits
#'
#' @slot margdata An S4 object of class \code{margtransf.class}. See \code{\link{margtransf}} for more details. 
#' @slot blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @slot nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @slot alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% confidence intervals. Default is \code{0.05}.
#' @slot gof A list containing the model and empirical exponential quantiles, and the lower and upper bound of the confidence interval.
#' 
#' @keywords internal
marggpd.class <- function(margdata, blocksize, nboot, alpha, marggpd){
  .marggpd.class(margdata = margdata,
                 blocksize = blocksize,
                 nboot = nboot,
                 alpha = alpha,
                 marggpd = marggpd)
}

#' Visualisation of the assessment of the Marginal Tail Fits
#'
#' @description Plot method for an S4 object returned by \code{\link{marggpd}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{marggpd}}.
#' 
#' @return A ggplot object showing the QQ-plot between the model and empirical generalised Pareto distribution quantiles.
#'
#' @rdname plotmarggpd
#'
#' @aliases plot,marggpd.class
#' 
#' @keywords internal
setMethod("plot", signature = list("marggpd.class"), function(x){
  X <- Y <- model <- empirical <- NULL # NULL them out to satisfy CRAN checks
  dfX <- data.frame("model" = x@marggpd$model[[1]], "empirical" = x@marggpd$empirica[[1]])
  ploygondfX <- data.frame("X" = c(rev(x@marggpd$model[[1]]), x@marggpd$model[[1]]),
                           "Y" = c(rev(x@marggpd$lower[[1]]), x@marggpd$upper[[1]]))
  dfY <- data.frame("model" = x@marggpd$model[[2]], "empirical" = x@marggpd$empirica[[2]])
  ploygondfY <- data.frame("X" = c(rev(x@marggpd$model[[2]]), x@marggpd$model[[2]]),
                           "Y" = c(rev(x@marggpd$lower[[2]]), x@marggpd$upper[[2]]))
  qqX <- ggplot(data = ploygondfX, aes(x = X, y = Y)) + geom_polygon(fill = "grey80", col = NA) +
    geom_point(data = dfX, mapping = aes(x = model, y = empirical)) + 
    geom_abline(col = 2, linewidth = 1) + 
    labs(x = "Model quantiles", y = "Empirical quantiles") +
    theme_minimal() +
    ggtitle("Marginal tail fit of X")
  qqY <- ggplot(data = ploygondfY, aes(x = X, y = Y)) + geom_polygon(fill = "grey80", col = NA) +
    geom_point(data = dfY, mapping = aes(x = model, y = empirical)) + 
    geom_abline(col = 2, linewidth = 1) + 
    labs(x = "Model quantiles", y = "Empirical quantiles") +
    theme_minimal() +
    ggtitle("Marginal tail fit of Y")
  grid.arrange(grobs = list(qqX, qqY), ncol = 2)
})


#' Assessing the Marginal Tail Fits
#' 
#' @name marggpd
#' 
#' @description \loadmathjax{}
#' Assessment of the marginal tail fits for each margin following the marginal transformation procedure \code{\link{margtransf}}.
#' 
#' @docType methods
#' 
#' @param margdata An S4 object of class \code{margtransf.class}. See \code{\link{margtransf}} for more details. 
#' @param blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @param nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @param alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% tolerance intervals. Default is \code{0.05}.
#' 
#' @return An object of S4 class \code{marggpd.class}. This object returns the arguments of the function and an extra slot \code{marggpd} which is a list containing: 
#' \item{model}{A list containing the model quantiles for each variable.} 
#' \item{empirical}{A list containing the empirical quantiles for each variable.}
#' \item{lower}{A list containing the lower bounds of the tolerance intervals for each variable.}
#' \item{upper}{A list containing the upper bounds of the tolerance intervals for each variable.}
#' 
#' @rdname marggpd
#' 
#' @details Let \mjeqn{X^{GPD}_{(i)}}{} denote the \mjeqn{i}{i}-th ordered increasing statistic 
#' \mjeqn{(i = 1, \ldots, n)}{} of the exceedances, i.e., \mjeqn{X^{GPD}= (X-u \mid X >u),}{} 
#' \mjeqn{n_{exc}}{} denote the sample size of these exceedances, and \mjeqn{F_{GPD}^{-1}}{} denote the 
#' inverse of the cumulative distribution function of a generalised Pareto distribution (GPD).
#' Function \code{plot} shows QQ plots between the model and empirical GPD quantiles for both variables, i.e, for 
#' the first variable points \mjeqn{\left(F^{-1}_{GPD}\left(\frac{i}{n_{exc}+1}\right) + u, X^{GPD}_{(i)} + u\right)}{}, 
#' along with the line \mjeqn{y=x}{}. 
#' 
#' Uncertainty on the empirical quantiles is obtained via a (block) bootstrap procedure and shown by the grey region on the plot.
#' A good fit is shown by agreement of model and empirical quantiles, i.e. points should lie close to the line \mjeqn{y=x}{}. 
#' In addition, line \mjeqn{y = x}{} should mainly lie within the \mjeqn{(1-\alpha)}{}\% tolerance intervals.
#' 
#' @aliases marggpd
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
#' # blocksize to account for temporal dependence
#' marggpd <- marggpd(margdata = margdata, blocksize = 10)
#' 
#' plot(marggpd)
#' 
#' # To see the the S4 object's slots
#' str(marggpd)
#' 
#' # To access the list of lists
#' marggpd@@marggpd
#' 
#' @export
#' 
marggpd <- function(margdata, blocksize = 1, nboot = 250, alpha = 0.05){
  if(!inherits(margdata, "margtransf.class")){
    stop("The margdata argument needs to be an object of class margtransf.class.")
  }
  data <- margdata@data[complete.cases(margdata@data), ]
  thresh <- margdata@thresh
  parameters <- margdata@parameters
  result <- marggpd.class(margdata = margdata, blocksize = blocksize,
                          nboot = nboot, alpha = alpha, marggpd = list())
  uncgpd <- sapply(1:dim(data)[2], function(i) gpdunc(data[, i], par = parameters[, i], 
                                                      thresh = thresh[i], blocksize = blocksize, 
                                                      nboot = nboot, alpha = alpha))
  result@marggpd <- list("model" = uncgpd[1, ], "empirical" = uncgpd[2, ],
                         "lower" = uncgpd[3, ], "upper" = uncgpd[4, ])
  return(result)
}

