jsmurf <- function(data, filter, family = c("jsmurf", "jsmurfPS", "jsmurfLR",
                                            "hjsmurf", "hjsmurfSPS", "hjsmurfLR"),
                   q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
                   locationCorrection = c("deconvolution", "constant", "none"),
                   output = c("onlyIdealization", "eachStep", "everything"), ...) {
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("data must be a finite numerical vector")
  }
  
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class lowpassFilter")
  }
  
  family <- match.arg(family)
  
  if (is.null(q)) {
    if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
      stop("alpha must be a probability, i.e. a single numeric between 0 and 1")
    }
    
    .removeAdditionalArgsGCV <- function(alpha, n, filter, family, ..., regularization, thresholdLongSegment,
                                         localEstimate, gridSize, windowFactorRefinement, report,
                                         suppressWarningNoDeconvolution)
      getCritVal(alpha = alpha, n = n, filter = filter, family = family, ...)
    q <- .removeAdditionalArgsGCV(alpha = alpha, n = length(data), filter = filter, family = family, ...)
  } else {
    if (!is.numeric(q) || !all(is.finite(q))) {
      stop("q must be a finite numeric")
    }
  }
  
  if (is.null(sd)) {
    if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
      sd <- stepR::sdrobnorm(data, lag = filter$len + 1L)
    }
  } else {
    if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
      if (!is.numeric(sd) || length(sd) != 1 || !is.finite(sd) || sd <= 0) {
        stop("sd must be a single positive finite numeric")
      }
    } else {
      warning("for heterogeneous familys no standard deviation has to be given, argument 'sd' will be ignored")
    }
  }
  
  if (!is.numeric(startTime) || length(startTime) != 1 || !is.finite(startTime)) {
    stop("startTime must be a single finite numeric")
  }
  
  locationCorrection <- match.arg(locationCorrection)
  
  output <- match.arg(output)
  
  if (output == "everything") {
    fit <- .stepJsmurf(data = data, filter = filter, family = family, q = q, sd = sd, startTime = startTime,
                       output = "everything")
  } else {
    fit <- .stepJsmurf(data = data, filter = filter, family = family, q = q, sd = sd, startTime = startTime,
                       output = "onlyFit")
  }
  
  .removeAdditionalArgsDL <- function(fit, data, filter, startTime, output, ..., family, q, alpha, sd, r, nq,
                                      options, stat, messages)
    deconvolveLocally(fit = fit, data = data, filter = filter, startTime = startTime,
                      output = output, ...)
  
  if (locationCorrection == "deconvolution") {
    if (output == "everything") {
      ret <- .removeAdditionalArgsDL(fit = fit, data = data, filter = filter,
                                     startTime = startTime, output = "everyGrid", ...)
    } else {
      ret <- .removeAdditionalArgsDL(fit = fit, data = data, filter = filter, startTime = startTime,
                                     output = "onlyIdealization", ...)
    }
  } else {
    ret <- fit
    
    if (locationCorrection == "constant") {
      if (output == "everything") {
        ret$fit$leftEnd[-1] <- ret$fit$leftEnd[-1] - filter$jump / filter$sr
        ret$fit$rightEnd[-length(ret$fit$rightEnd)] <- ret$fit$rightEnd[-length(ret$fit$rightEnd)] - filter$jump / filter$sr
      } else {
        ret$leftEnd[-1] <- ret$leftEnd[-1] - filter$jump / filter$sr
        ret$rightEnd[-length(ret$rightEnd)] <- ret$rightEnd[-length(ret$rightEnd)] - filter$jump / filter$sr
      }
    }
  }
  
  if (output != "onlyIdealization") {
    if (output == "everything") {
      if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
        ret <- list(idealization = ret, fit = fit$fit, q = q, filter = filter, sd = sd)
      } else {
        ret <- list(idealization = ret, fit = fit$fit, q = q, filter = filter)
      }
    } else {
      if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
        ret <- list(idealization = ret, fit = fit, q = q, filter = filter, sd = sd)
      } else {
        ret <- list(idealization = ret, fit = fit, q = q, filter = filter)
      }
    }
  }
  
  ret
}

.stepJsmurf <- function(data, filter, family, q, sd, startTime, output) {
  time <- startTime + seq(along = data) / filter$sr
  
  if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
    fit <- stepR::stepFit(y = data, x = time, x0 = startTime, family = family, q = q, filter = filter, sd = sd)
    ret <- stepR::stepblock(value = fit$value, leftEnd = c(startTime, fit$rightEnd[-length(fit$rightEnd)]),
                            rightEnd = fit$rightEnd, x0 = attr(fit, "x0"))
    if (output == "everything") {
      ret <- list(fit = ret, q = q, filter = filter, sd = sd)
    }
  } else {
    fit <- stepR::stepFit(y = data, x = time, x0 = startTime, family = family, q = q, filter = filter)
    ret <- stepR::stepblock(value = fit$value, leftEnd = c(startTime, fit$rightEnd[-length(fit$rightEnd)]),
                            rightEnd = fit$rightEnd, x0 = attr(fit, "x0"))
    if (output == "everything") {
      ret <- list(fit = ret, q = q, filter = filter)
    }
  }
  
  ret
}
