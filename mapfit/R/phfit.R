#' PH fitting with point data
#' 
#' Estimates PH parameters from point data.
#' 
#' @param ph An object of R6 class for PH. The estimation algorithm is selected depending on this class.
#' @param x A vector for point data.
#' @param weights A vector of weights for points.
#' @param ... Further options for fitting methods.
#' @return
#' Returns a list with components, which is an object of S3 class \code{phfit.result};
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
#' ## make sample
#' wsample <- rweibull(n=100, shape=2, scale=1)
#' 
#' ## PH fitting for general PH
#' (result1 <- phfit.point(ph=ph(2), x=wsample))
#' 
#' ## PH fitting for CF1
#' (result2 <- phfit.point(ph=cf1(2), x=wsample))
#' 
#' ## PH fitting for hyper Erlang
#' (result3 <- phfit.point(ph=herlang(3), x=wsample))
#' 
#' ## mean
#' ph.mean(result1$model)
#' ph.mean(result2$model)
#' ph.mean(result3$model)
#' 
#' ## variance
#' ph.var(result1$model)
#' ph.var(result2$model)
#' ph.var(result3$model)
#' 
#' ## up to 5 moments 
#' ph.moment(5, result1$model)
#' ph.moment(5, result2$model)
#' ph.moment(5, result3$model)
#' 
#' @export

phfit.point <- function(ph, x, weights, ...) {
  call <- match.call()
  ph <- ph$copy()
  
  options <- emoptions()
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  
  data <- data.frame.phase.time(x=x, weights=weights)
  if (options$initialize == TRUE) {
    ph$init(data, options)
  }
  tres <- system.time(result <- ph$emfit(data, options, ...))
  result <- c(result, list(model=ph, aic=-2*(result$llf - ph$df()), df=ph$df(),
                           data=data, ctime=tres[1], options=options, call=call))
  class(result) <- "phfit.result"
  result
}

#' PH fitting with grouped data
#' 
#' Estimates PH parameters from grouped data.
#' 
#' @param ph An object of R6 class. The estimation algorithm is selected depending on this class.
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
#' Returns a list with components, which is an object of S3 class \code{phfit.result};
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
#' \item{options}{a list of options used in the fitting.}
#' \item{call}{the matched call.}
#' 
#' @note
#' In this method, we can handle truncated data using \code{NA} and \code{Inf};
#' \code{phfit.group(ph=cf1(5), counts=c(countsdata, NA), breaks=c(breakdata, +Inf))}
#' \code{NA} means missing of count data at the corresponding interval, and \code{Inf} is allowed to put 
#' the last of breaks or intervals which represents a special interval [the last break point,infinity).
#' 
#' @examples
#' ## make sample
#' wsample <- rweibull(n=100, shape=2, scale=1)
#' wgroup <- hist(x=wsample, breaks="fd", plot=FALSE)
#' 
#' ## PH fitting for general PH
#' (result1 <- phfit.group(ph=ph(2), counts=wgroup$counts, breaks=wgroup$breaks))
#' 
#' ## PH fitting for CF1
#' (result2 <- phfit.group(ph=cf1(2), counts=wgroup$counts, breaks=wgroup$breaks))
#' 
#' ## PH fitting for hyper Erlang
#' (result3 <- phfit.group(ph=herlang(3), counts=wgroup$counts, breaks=wgroup$breaks))
#' 
#' ## mean
#' ph.mean(result1$model)
#' ph.mean(result2$model)
#' ph.mean(result3$model)
#' 
#' ## variance
#' ph.var(result1$model)
#' ph.var(result2$model)
#' ph.var(result3$model)
#' 
#' ## up to 5 moments 
#' ph.moment(5, result1$model)
#' ph.moment(5, result2$model)
#' ph.moment(5, result3$model)
#' 
#' @export

phfit.group <- function(ph, counts, breaks, intervals, instants, ...) {
  call <- match.call()
  ph <- ph$copy()
  
  options <- emoptions()
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("unknown names in control: ", paste(noNms, collapse = ", "))
  
  data <- data.frame.phase.group(counts=counts, breaks=breaks,
                                 intervals=intervals, instants=instants)
  
  if (options$initialize == TRUE) {
    ph$init(data, options)
  }
  tres <- system.time(result <- ph$emfit(data, options, ...))
  result <- c(result, list(model=ph, aic=-2*(result$llf - ph$df()), df=ph$df(),
                           data=data, ctime=tres[1], options=options, call=call))
  class(result) <- "phfit.result"
  result
}

#' PH fitting with density function
#' 
#' Estimates PH parameters from density function.
#' 
#' @param ph An object of R6 class. The estimation algorithm is selected depending on this class.
#' @param f A function object for a density function.
#' @param deformula An object for formulas of numerical integration.
#' It is not necessary to change it when the density function is defined on
#' the positive domain [0,infinity).
#' @param weight.zero A absolute value which is regarded as zero in numerical integration.
#' @param weight.reltol A value for precision of numerical integration.
#' @param start.divisions A value for starting value of divisions in deformula.
#' @param max.iter A value for the maximum number of iterations to increase divisions in deformula.
#' @param ... Options for EM steps, which is also used to send the arguments to density function.
#' 
#' @return
#' Returns a list with components, which is an object of S3 class \code{phfit.result};
#' \item{model}{an object for estimated PH class.}
#' \item{llf}{a value of the maximum log-likelihood (a negative value of the cross entropy).}
#' \item{df}{a value of degrees of freedom of the model.}
#' \item{KL}{a value of Kullback-Leibler divergence.}
#' \item{iter}{the number of iterations.}
#' \item{convergence}{a logical value for the convergence of estimation algorithm.}
#' \item{ctime}{computation time (user time).}
#' \item{data}{an object for data class}
#' \item{aerror}{a value of absolute error for llf at the last step of algorithm.}
#' \item{rerror}{a value of relative error for llf at the last step of algorithm.}
#' \item{options}{a list of options.}
#' \item{call}{the matched call.}
#' 
#' @note
#' Any of density function can be applied to the argument \code{f}, where
#' \code{f} should be defined \code{f <- function(x, ...)}.
#' The first argument of \code{f} should be an integral parameter.
#' The other parameters are set in the argument \code{...} of \code{phfit.density}.
#' The truncated density function can also be used directly.
#' 
#' @examples
#' ####################
#' ##### truncated density
#' ####################
#' 
#' ## PH fitting for general PH
#' (result1 <- phfit.density(ph=ph(2), f=dnorm, mean=3, sd=1))
#' 
#' ## PH fitting for CF1
#' (result2 <- phfit.density(ph=cf1(2), f=dnorm, mean=3, sd=1))
#' 
#' ## PH fitting for hyper Erlang
#' (result3 <- phfit.density(ph=herlang(3), f=dnorm, mean=3, sd=1))
#' 
#' ## mean
#' ph.mean(result1$model)
#' ph.mean(result2$model)
#' ph.mean(result3$model)
#' 
#' ## variance
#' ph.var(result1$model)
#' ph.var(result2$model)
#' ph.var(result3$model)
#' 
#' ## up to 5 moments 
#' ph.moment(5, result1$model)
#' ph.moment(5, result2$model)
#' ph.moment(5, result3$model)
#' 
#' @export

phfit.density <- function(
    ph, f, deformula = deformula.zeroinf, weight.zero = 1.0e-12,
    weight.reltol = 1.0e-8, start.divisions = 8, max.iter = 12,
    ...) {
  call <- match.call()
  ph <- ph$copy()
  
  x <- deformula(f, ..., zero.eps = weight.zero,
                 rel.tol = weight.reltol,
                 start.divisions = start.divisions, max.iter = max.iter)
  ll <- sum(x$w * log(f(x$x, ...)))
  data <- data.frame.phase.time(x=x$x, weights=x$w)
  
  options <- emoptions()
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con
  # if (length(noNms <- namc[!namc %in% nmsC])) 
  #   warning("unknown names in control: ", paste(noNms, collapse = ", "))
  
  if (options$initialize == TRUE) {
    ph$init(data, options)
  }
  tres <- system.time(result <- ph$emfit(data, options, ...))
  result <- c(result, list(model=ph, KL=x$h * (ll-result$llf), df=ph$df(),
                           data=data, ctime=tres[1], options=options, call=call))
  class(result) <- "phfit.result.density"
  result
}

#' @aliases phfit.point phfit.group
#' @export

print.phfit.result <- function (x, ...) {
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

#' @aliases phfit.density
#' @export

print.phfit.result.density <- function (x, ...) {
  cat("\n")
  cat(sprintf("Maximum LLF: %f\n", x$llf))
  cat(sprintf("DF: %d\n", x$df))
  cat(sprintf("KL: %f\n", x$KL))
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
