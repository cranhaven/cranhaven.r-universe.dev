#' @name confBands.OEFPIL
#' @title Confidence and prediction bands for OEFPIL object
#' @description Function calculates pointwise confidence bands and prediction bands of estimated function from an object of class \code{"OEFPIL"}.
#' @usage ## S3 method for class 'OEFPIL'
#'    confBands(object, xx, signif.level = 0.05, new.obs.variance)
#'
#' @param object an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param xx a sequence of x-coordinates of points for computing confidence and prediction intervals. If missing, the default sequence \code{seq(from = min(x), to = max(x), length.out = 301)} is used.
#' @param signif.level  a numerical value or a vector of significance levels for confidence bands. If missing, the default value 0.05 is used.
#' @param new.obs.variance the variance of a new observation for prediction interval computing.
#'
#' @details An argument \code{signif.level} can be one numerical value or vector of numerical values of significance levels for confidence intervals.
#'
#' If \code{new.obs.variance} is not defined by user, the average variance in the dependent variable is used to compute prediction intervals.
#'
#' @return Returns an object of type list containing the following components.
#'
#' \item{xx}{a numerical vector of points where intervals are calculated.}
#' \item{yy}{a numerical vector with values of estimated function in \code{xx}.}
#' \item{PointwiseCB}{a matrix of confidence intervals at points \code{xx}.}
#' \item{PredictCB}{a matrix of prediction intervals at points \code{xx}.}
#'
#' @seealso \code{\link{OEFPIL}}, \code{\link{plot.OEFPIL}}
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' ##Use of confBands function with default parameters
#' a <- confBands(st1)
#' str(a)
#'
#' ##Computing two different confidence bands in one step
#' b <- confBands(st1, signif.level = c(0.01,0.05))
#' str(b)
#'
#' @export
confBands <- function(object, xx, signif.level = 0.05, new.obs.variance) {
  UseMethod("confBands")
}

#' @export

confBands.OEFPIL <- function(object, xx, signif.level = 0.05, new.obs.variance) {
  ## This is for calculating confidence bands of estimated function from OEFPIL.
  ## object           . . . output from OEFPIL()
  ## xx               . . . in these points we calculate CI (confidence intervals) or
  ##                        CB (conf. bands)
  ## new.obs.variance . . . a variance of the new observation;
  ##                        it is needed for prediction intervals.

  LOF <- object$contents$LOF ## list of functions
  x <- object$contents[[3]] ## x data
  y <- object$contents[[4]] ## y data
  CM <- object$contents$CM ## covariance matrix of the data

  cov_m <- object$cov.m_Est ## estimate of covariance matrix of parameters
  l <- length(object$contents$names.of.parameters) ## number of parameters

  lst.parameters <- object[1:l]
  names(lst.parameters) <- object$contents$names.of.parameters
  ## estimate of the parameters
  
  lst.parameters_previous.step <- object[(2*l+6):(3*l+5)]
  names(lst.parameters_previous.step) <- object$contents$names.of.parameters
  ## estimate of the parameters from the previous step

  if (IsListOK(lst.parameters) && IsListOK(lst.parameters_previous.step) && IsListOK(cov_m)) {

    if (missing(xx)) {
      xx <- seq(from = min(x), to = max(x), length.out = 301)
    }
    yy <- sapply(xx, function(val, LP){do.call(LOF[[1]], args=c(val, LP))}, lst.parameters_previous.step)
    ## the middle of the confidence interval
    
    true.yy <- sapply(xx, function(val, LP){do.call(LOF[[1]], args=c(val, LP))}, lst.parameters)
    ## the real etimate of a given function

    Omega <- sapply(1:l, function(i) {
      sapply(xx, function(val, LP){do.call(LOF[[2+i]], args=c(val, LP))}, lst.parameters_previous.step)
    })
    ## i-th row of matrix is value of vector omega in the point xx[i]

    variance <- apply(((Omega %*% cov_m) * Omega), 1, sum)

    if (missing(new.obs.variance)) {

      n <- length(diag(CM)) / 2
      new.obs.variance <- mean(diag(CM)[(n+1):(2*n)])
      ## "estimation" of variance of the new observation (needed for prediction interval)

    }

    sl <- sort(c(signif.level/2, 1 - signif.level/2), decreasing = FALSE)

    d <- length(signif.level)
    k <- length(xx)

    PCB_lwr <- matrix(rep(yy, d), k, d) + matrix(rep(qnorm(sl[1:d]), k), k, d, byrow = TRUE) * sqrt(variance)
    PCB_upr <- matrix(rep(yy, d), k, d) + matrix(rep(qnorm(sl[(d+1):(2*d)]), k), k, d, byrow = TRUE) * sqrt(variance)
    ## pointwise confidence band

    PredictCB_lwr <- matrix(rep(yy, d), k, d) + matrix(rep(qnorm(sl[1:d]), k), k, d, byrow = T) * sqrt(variance + new.obs.variance)
    PredictCB_upr <- matrix(rep(yy, d), k, d) + matrix(rep(qnorm(sl[(d+1):(2*d)]), k), k, d, byrow = T) * sqrt(variance + new.obs.variance)
    ## pointwise confidence band

    PointwiseCB <- cbind(PCB_lwr, PCB_upr)
    PredictCB <- cbind(PredictCB_lwr, PredictCB_upr)

    colnames(PointwiseCB) <- paste(round(sl * 100, 2), "%")
    colnames(PredictCB) <- paste(round(sl * 100, 2), "%")

    return(invisible(list(xx = xx, yy = true.yy, PointwiseCB = PointwiseCB, PredictCB = PredictCB)))
  }
}



