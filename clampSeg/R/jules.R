jules <- function(data, filter, q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
                  output = c("onlyIdealization", "eachStep", "everything"), ...) {
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class lowpassFilter")
  }
  
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("data must be a finite numerical vector")
  }
  
  if (!is.numeric(startTime) || length(startTime) != 1 || !is.finite(startTime)) {
    stop("startTime must be a single finite numeric")
  }

  if (is.null(sd)) {
    sd <- stepR::sdrobnorm(data, lag = filter$len + 1L)
  } else {
    if (!is.numeric(sd) || length(sd) != 1 || !is.finite(sd) || sd <= 0) {
      stop("sd must be a single positive finite numeric")
    }
  }
  
  if (is.null(q)) {
    if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
      stop("alpha must be a probability, i.e. a single numeric between 0 and 1")
    }
    
    .removeAdditionalArgsGCV <- function(alpha, n, filter, ..., regularization, thresholdLongSegment,
                                        localEstimate, gridSize, windowFactorRefinement, report,
                                        suppressWarningNoDeconvolution)
      getCritVal(alpha = alpha, n = n, filter = filter, ...)
    q <- .removeAdditionalArgsGCV(alpha = alpha, n = length(data), filter = filter, ...)
  } else {
    if (!is.numeric(q) || length(q) != 1 || !is.finite(q)) {
      stop("q must be a single finite numeric")
    }
  }
  
  output <- match.arg(output)
  
  .removeAdditionalArgsDL <- function(fit, data, filter, startTime, output, ..., nq, stat, r, options, messages)
    deconvolveLocally(fit = fit, data = data, filter = filter, startTime = startTime,
                      output = output, ...)
  if (output == "everything") {
    fit <- stepDetection(data = data, startTime = startTime, filter = filter, sd = sd, q = q,
                         output = "everything")
    ret <- .removeAdditionalArgsDL(fit = fit, data = data, filter = filter,
                                  startTime = startTime, output = "everyGrid", ...)
  } else {
    fit <- stepDetection(data = data, startTime = startTime, filter = filter, sd = sd, q = q)
    ret <- .removeAdditionalArgsDL(fit = fit, data = data, filter = filter, startTime = startTime,
                                  output = "onlyIdealization", ...)
  }
  
  if (output != "onlyIdealization") {
    if (output == "everything") {
      ret <- list(idealization = ret, fit = fit$fit, stepfit = fit$stepfit, q = q, filter = filter,
                  sd = sd)
    } else {
      ret <- list(idealization = ret, fit = fit, q = q, filter = filter, sd = sd)
    }
  }
  
  ret
}
  