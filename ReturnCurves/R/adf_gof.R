.adf_gof.class <- setClass("adf_gof.class", representation(adf = "adf_est.class",
                                                           ray = "numeric",
                                                           blocksize = "numeric",
                                                           nboot = "numeric",
                                                           alpha = "numeric",
                                                           gof = "list"))

#' An S4 class to represent the Goodness-of-Fit of the Angular Dependence Function estimates
#'
#' @slot adf An S4 object of class \code{adf_est.class}.
#' @slot ray \loadmathjax{} Ray \mjeqn{\omega}{} to be considered on the goodness of fit assessment.
#' @slot blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @slot nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @slot alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% confidence intervals. Default is \code{0.05}.
#' @slot gof A list containing the model and empirical exponential quantiles, and the lower and upper bound of the confidence interval.
#' 
#' @keywords internal
adf_gof.class <- function(adf, ray, blocksize, nboot, alpha, gof){
  .adf_gof.class(adf = adf,
                 ray = ray,
                 blocksize = blocksize,
                 nboot = nboot,
                 alpha = alpha,
                 gof = gof)
}

#' Visualisation of the Goodness-of-Fit of the Angular Dependence Function estimates
#'
#' @description Plot method for an S4 object returned by \code{\link{adf_gof}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{adf_gof}}.
#' 
#' @return A ggplot object showing the QQ-plot between the model and empirical exponential quantiles.
#'
#' @rdname plotadfgof
#'
#' @aliases plot,adf_gof.class
#' 
#' @keywords internal
setMethod("plot", signature = list("adf_gof.class"), function(x){
  X <- Y <- model <- empirical <- NULL # NULL them out to satisfy CRAN checks
  df <- as.data.frame(x@gof)
  ploygondf <- data.frame("X" = c(rev(x@gof$model), x@gof$model),
                          "Y" = c(rev(x@gof$lower), x@gof$upper))
  ggplot(data = ploygondf, aes(x = X, y = Y)) + geom_polygon(fill = "grey80", col = NA) +
    geom_point(data = df, mapping = aes(x = model, y = empirical)) + 
    geom_abline(col = 2, linewidth = 1) + 
    labs(x = "Model quantiles", y = "Empirical quantiles") +
    theme_minimal() +
    ggtitle(bquote("Goodness of fit of" ~ hat(lambda)(omega) ~ "at" ~ omega ~ "=" ~ .(x@ray)))
})

#' Goodness of fit of the Angular Dependence function estimates
#' 
#' @name adf_gof
#' 
#' @description \loadmathjax{}
#' Assessment of the goodness of fit of the angular dependence function estimates \mjeqn{\lambda(\omega)}{} following the procedure of \insertCite{MurphyBarltropetal2024;textual}{ReturnCurves}.
#' 
#' @docType methods
#' 
#' @param adf An S4 object of class \code{adf_est.class}. See \code{\link{adf_est}} for more details.
#' @param ray Ray \mjeqn{\omega}{} to be considered on the goodness of fit assessment.
#' @param blocksize Size of the blocks for the block bootstrap procedure. If \code{1} (default), then a standard bootstrap approach is applied.
#' @param nboot Number of bootstrap samples to be taken. Default is \code{250} samples.
#' @param alpha Significance level to compute the \mjeqn{(1-\alpha)}{}\% tolerance intervals. Default is \code{0.05}.
#' 
#' @return An object of S4 class \code{adf_gof.class}. This object returns the arguments of the function and an extra slot \code{gof} which is a list containing: 
#' \item{model}{A vector containing the model quantiles.} 
#' \item{empirical}{A vector containing the empirical quantiles.}
#' \item{lower}{A vector containing the lower bound of the tolerance interval.}
#' \item{upper}{A vector containing the upper bound of the tolerance interval.}
#' 
#' @details Define the min-projection variable as \mjeqn{t^1_\omega = t_\omega - u_\omega | t_\omega > u_\omega}{}, then
#' variable \mjeqn{\lambda(\omega)T^1_\omega \sim Exp(1)}{} as \mjeqn{u_\omega \to \infty}{} for all \mjeqn{\omega \in [0,1]}{}. 
#'
#' Let \mjeqn{F^{-1}_E}{} denote the inverse of the cumulative distribution function of a standard exponential variable and \mjeqn{T^1_{(i)}}{} denote the \mjeqn{i}{i}-th ordered increasing statistic, \mjeqn{i = 1, \ldots, n}{}. 
#' Function \code{plot} shows a QQ plot between the model and empirical exponential quantiles, i.e. points \mjeqn{\left(F^{-1}_E\left(\frac{i}{n+1}\right), T^1_{(i)}\right)}{},
#' along with the line \mjeqn{y=x}{}. Uncertainty is obtained via a (block) bootstrap procedure and shown by the grey region on the plot.
#' A good fit is shown by agreement of model and empirical quantiles, i.e. points should lie close to the line \mjeqn{y=x}{}. 
#' In addition, line \mjeqn{y = x}{} should mainly lie within the \mjeqn{(1-\alpha)}{}\% tolerance intervals.
#' 
#' We note that, if the grid for \mjeqn{\omega}{} used to estimate the Angular Dependence Function (ADF) does not contain \code{ray}, then the closest \mjeqn{\omega}{w} in the grid is used to assess the goodness-of-fit of the ADF.
#' 
#' @note It is recommended to assess the goodness-of-fit of \mjeqn{\lambda(\omega)}{} for a few values of \mjeqn{\omega}{w}.
#' 
#' @rdname adf_gof
#' 
#' @references \insertAllCited{}
#' 
#' @aliases adf_gof
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
#' lambda <- adf_est(margdata = margdata, method = "hill")
#' 
#' # blocksize to account for temporal dependence
#' gof <- adf_gof(adf = lambda, ray = 0.4, blocksize = 10)
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
adf_gof <- function(adf, ray, blocksize = 1, nboot = 250, alpha = 0.05){
  if(!inherits(adf, "adf_est.class")){
    stop("The adf argument needs to be an object of class adf_est.class.")
  }
  w <- adf@w
  data <- adf@dataexp
  lambda <- adf@adf
  q <- adf@q
  if(ray < 0 | ray > 1){
    stop("The ray should be in [0,1].")
  }
  if(nboot < 1 | nboot %% 1 != 0){
    stop("The number of bootstrap samples needs to be a positive integer.")
  }
  if(alpha < 0 | alpha > 1){
    stop("The significance level needs to be in [0, 1].")
  }
  if(alpha > 0.5){
    warning("This will lead to a confidence interval smaller than 50%. Perhaps you mean 1-alpha.")
  }
  raydif <- abs(ray - w)
  w_ind <- which.min(raydif)
  result <- adf_gof.class(adf = adf, ray = ray, blocksize = blocksize,
                          nboot = nboot, alpha = alpha, gof = list())
  min_proj <- minproj_lambda(data = data, w = w[w_ind], q_minproj = q)
  excdata <- (min_proj$minproj - min_proj$thresh)[min_proj$minproj > min_proj$thresh]
  excdata <- lambda[w_ind] * excdata
  nexcdata <- length(excdata)
  empirical_quantile <- sort(excdata)
  model_quantile <- qexp((1:nexcdata) / (nexcdata + 1), rate = 1)
  empirical_quantile_boot <- matrix(NA, nrow = nboot, ncol = length(empirical_quantile))
  for(i in 1:nboot){
    bdata <- block_bootstrap_function(data = excdata, k = blocksize, n = nexcdata)
    empirical_quantile_boot[i, ] <- sort(bdata)
  }
  ub <- apply(empirical_quantile_boot, 2, quantile, probs = 1 - alpha/2)
  lb <- apply(empirical_quantile_boot, 2, quantile, probs = alpha/2)
  result@gof <- list("model" = model_quantile, "empirical" = empirical_quantile, "lower" = lb, "upper" = ub)
  return(result)
}
