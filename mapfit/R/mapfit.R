#' MAP fitting with point data
#' 
#' Estimates MAP parameters from point data.
#' 
#' @param map An object for MAP. The estimation algorithm is selected depending on this class.
#' @param x A vector for point data.
#' @param intervals A vector for intervals.
#' @param ... Further options for fitting methods.
#' @return
#' Returns a list with components, which is an object of S3 class \code{mapfit.result};
#' \item{model}{an object for estimated PH class.}
#' \item{llf}{a value of the maximum log-likelihood.}
#' \item{df}{a value of degrees of freedom of the model.}
#' \item{aic}{a value of Akaike information criterion.}
#' \item{iter}{the number of iterations.}
#' \item{convergence}{a logical value for the convergence of estimation algorithm.}
#' \item{ctime}{computation time (user time).}
#' \item{data}{an object for data class}
#' \item{aerror}{a value of absolute error for llf at the last step of algorithm.}
#' \item{rerror}{a value of relative error for llf at the last step of algorithm.}
#' \item{options}{a list of options used for fitting.}
#' \item{call}{the matched call.}
#' 
#' @examples 
#' ## load trace data
#' data(BCpAug89)
#' BCpAug89s <- head(BCpAug89, 50)
#' 
#' ## MAP fitting for general MAP
#' (result1 <- mapfit.point(map=map(2), x=cumsum(BCpAug89s)))
#'
#' ## MAP fitting for MMPP
#' (result2 <- mapfit.point(map=mmpp(2), x=cumsum(BCpAug89s)))
#' 
#' ## MAP fitting for ER-HMM
#' (result3 <- mapfit.point(map=erhmm(3), x=cumsum(BCpAug89s)))
#' 
#' ## marginal moments for estimated MAP
#' map.mmoment(k=3, map=result1$model)
#' map.mmoment(k=3, map=result2$model)
#' map.mmoment(k=3, map=result3$model)
#' 
#' ## joint moments for estimated MAP
#' map.jmoment(lag=1, map=result1$model)
#' map.jmoment(lag=1, map=result2$model)
#' map.jmoment(lag=1, map=result3$model)
#' 
#' ## lag-k correlation
#' map.acf(map=result1$model)
#' map.acf(map=result2$model)
#' map.acf(map=result3$model)
#' 
#' @export

mapfit.point <- function(map, x, intervals, ...) {
  call <- match.call()
  map <- map$copy()

  options <- emoptions()
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  
  data <- data.frame.map.time(time=x, intervals=intervals)
  if (options$initialize == TRUE) {
    map$init(data, options)
  }
  tres <- system.time(result <- map$emfit(data, options, ...))
  result <- c(result, list(model=map, aic=-2*(result$llf - map$df()), df=map$df(),
                           data=data, ctime=tres[1], options=options, call=call))
  class(result) <- "mapfit.result"
  result
}

#' MAP fitting with grouped data
#'
#' Estimates MAP parameters from grouped data.
#'
#' @param map An object of R6 class. The estimation algorithm is selected depending on this class.
#' @param counts A vector of the number of points in intervals.
#' @param breaks A vector for a sequence of points of boundaries of intervals.
#' This is equivalent to \code{c(0,cumsum(intervals))}.
#' If this is missing, it is assigned to \code{0:length(counts)}.
#' @param intervals A vector of time lengths for intervals.
#' This is equivalent to \code{diff(breaks)}).
#' If this is missing, it is assigned to \code{rep(1,length(counts))}.
#' @param instants A vector of integers to indicate whether sample is drawn at
#' the last of interval. If instant is 1, a sample is drawn at the last of interval.
#' If instant is 0, no sample is drawn at the last of interval.
#' By using instant, point data can be expressed by grouped data.
#' If instant is missing, it is given by \code{rep(0L,length(counts))}, i.e.,
#' there are no samples at the last of interval.
#' @param ... Further options for EM steps.
#' @return
#' Returns a list with components, which is an object of S3 class \code{mapfit.result};
#' \item{model}{an object for estimated MAP class.}
#' \item{llf}{a value of the maximum log-likelihood.}
#' \item{df}{a value of degrees of freedom of the model.}
#' \item{aic}{a value of Akaike information criterion.}
#' \item{iter}{the number of iterations.}
#' \item{convergence}{a logical value for the convergence of estimation algorithm.}
#' \item{ctime}{computation time (user time).}
#' \item{data}{an object for data class}
#' \item{aerror}{a value of absolute error for llf at the last step of algorithm.}
#' \item{rerror}{a value of relative error for llf at the last step of algorithm.}
#' \item{options}{a list of options used in the fitting.}
#' \item{call}{the matched call.}
#'
#' @examples 
#' ## load trace data
#' data(BCpAug89)
#' BCpAug89s <- head(BCpAug89, 50)
#' 
#' ## make grouped data
#' BCpAug89.group <- hist(cumsum(BCpAug89s),
#'                          breaks=seq(0, 0.15, 0.005),
#'                          plot=FALSE)
#'                          
#' ## MAP fitting for general MAP
#' (result1 <- mapfit.group(map=map(2),
#'                         counts=BCpAug89.group$counts,
#'                         breaks=BCpAug89.group$breaks))
#' ## MAP fitting for MMPP
#' (result2 <- mapfit.group(map=mmpp(2),
#'                          counts=BCpAug89.group$counts,
#'                          breaks=BCpAug89.group$breaks))
#'                          
#' ## MAP fitting with approximate MMPP
#' (result3 <- mapfit.group(map=gmmpp(2),
#'                          counts=BCpAug89.group$counts,
#'                          breaks=BCpAug89.group$breaks))
#'
#' ## marginal moments for estimated MAP
#' map.mmoment(k=3, map=result1$model)
#' map.mmoment(k=3, map=result2$model)
#' map.mmoment(k=3, map=result3$model)
#' 
#' ## joint moments for estimated MAP
#' map.jmoment(lag=1, map=result1$model)
#' map.jmoment(lag=1, map=result2$model)
#' map.jmoment(lag=1, map=result3$model)
#' 
#' ## lag-k correlation
#' map.acf(map=result1$model)
#' map.acf(map=result2$model)
#' map.acf(map=result3$model)
#' 
#' @export

mapfit.group <- function(map, counts, breaks, intervals, instants, ...) {
  call <- match.call()
  map <- map$copy()
  
  options <- emoptions()
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con
  if (length(noNms <- namc[!namc %in% nmsC]))
    warning("unknown names in control: ", paste(noNms, collapse = ", "))

  data <- data.frame.map.group(counts=counts, breaks=breaks,
                               intervals=intervals, instants=instants)

  if (options$initialize == TRUE) {
    map$init(data, options)
  }
  tres <- system.time(result <- map$emfit(data, options, ...))
  result <- c(result, list(model=map, aic=-2*(result$llf - map$df()), df=map$df(),
                           data=data, ctime=tres[1], options=options, call=call))
  class(result) <- "mapfit.result"
  result
}

#' @aliases mapfit.point mapfit.group
#' @export

print.mapfit.result <- function (x, ...) {
  cat("\n")
  cat(sprintf("Maximum LLF: %f\n", x$llf))
  cat(sprintf("DF: %d\n", x$df))
  cat(sprintf("AIC: %f\n", x$aic))
  cat(sprintf("Iteration:  %d / %d\n", x$iter, x$options$maxiter))
  cat(sprintf("Computation time (user): %f\n", x$ctime))
  cat(sprintf("Convergence: %s\n", x$convergence))
  cat(sprintf("Error (abs): %e (tolerance %e)\n", x$aerror, x$options$abstol))
  cat(sprintf("Error (rel): %e (tolerance %e)\n", x$rerror, x$options$reltol))
  cat("\n")
  x$model$print(...)
  cat("\n\n")
  invisible(x)
}
