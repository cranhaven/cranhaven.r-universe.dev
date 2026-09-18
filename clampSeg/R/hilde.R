hilde <- function(data, filter, family = c("hjsmurf", "hjsmurfSPS", "hjsmurfLR",
                                           "jsmurf", "jsmurfPS", "jsmurfLR"),
                  method = c("2Param", "LR"), q1 = NULL, alpha1 = 0.01, q2 = NULL, alpha2 = 0.04, 
                  sd = NULL, startTime = 0,
                  output = c("onlyIdealization", "eachStep", "everything"), ...) {
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("data must be a finite numerical vector")
  }
  
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class lowpassFilter")
  }
  
  family <- match.arg(family)
  method <- match.arg(method)
  
  if (method == "2Param") {
    if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
      warning(paste0("family '", family, "' assumes homogenous noise, but method '",
                     method, "' is designed for heterogeneous noise"))
    }
  } else {
    if (family %in% c("hjsmurf", "hjsmurfSPS", "hjsmurfLR")) {
      warning(paste0("family '", family, "' assumes heterogeneous noise, but method '",
                     method, "' is designed for homogeneous noise"))
    }
  }
  
  if (is.null(q1)) {
    if (!is.numeric(alpha1) || length(alpha1) != 1 || !is.finite(alpha1) || alpha1 <= 0 || alpha1 >= 1) {
      stop("alpha1 must be a probability, i.e. a single numeric between 0 and 1")
    }
    
    .removeAdditionalArgsGCV1 <- function(alpha, n, filter, family, ..., lengths, thresholdLongSegment, localValue,
                                          localVar, regularization, gridSize, windowFactorRefinement, report,
                                          suppressWarningNoDeconvolution, localList)
      getCritVal(alpha = alpha, n = n, filter = filter, family = family, ...)
    q1 <- .removeAdditionalArgsGCV1(alpha = alpha1, n = length(data), filter = filter, family = family, ...)
  } else {
    if (!is.numeric(q1) || !all(is.finite(q1))) {
      stop("q1 must be a finite numeric")
    }
  }
  
  if (is.null(q2)) {
    if (!is.numeric(alpha2) || length(alpha2) != 1 || !is.finite(alpha2) || alpha2 <= 0 || alpha2 >= 1) {
      stop("alpha2 must be a probability, i.e. a single numeric between 0 and 1")
    }
    
    .removeAdditionalArgsGCV2 <- function(alpha, n, filter, family, ..., gridSize, windowFactorRefinement, report,
                                          regularization = NULL) {
      if (!is.null(regularization) && is.list(regularization)) {
        regularization <- regularization[[1]]
        ret <- getCritVal(alpha = alpha, n = n, filter = filter, family = family, regularization = regularization, ...)
      } else {
        ret <- getCritVal(alpha = alpha, n = n, filter = filter, family = family, ...)
      }
      ret
    }

    q2 <- .removeAdditionalArgsGCV2(alpha = alpha2, n = length(data), filter = filter, family = method, ...)
  } else {
    if (!is.numeric(q2) || !all(is.finite(q2))) {
      stop("q2 must be a finite numeric")
    }
  }
  
  if (!is.numeric(startTime) || length(startTime) != 1 || !is.finite(startTime)) {
    stop("startTime must be a single finite numeric")
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
      warning("for heterogeneous families no standard deviation has to be given, argument 'sd' will be ignored")
    }
  }
  
  output <- match.arg(output)
  
  .removeAdditionalArgsImprove <- function(data, fit, filter, method, q, alpha, startTime, 
                                           output, ..., family, sd, r, nq, options, stat, messages) {
    improveSmallScales(data = data, fit = fit, filter = filter, method = method, q = q, alpha = alpha,
                       startTime = startTime, output = output, ...)
  }
  
  if (output == "everything") {
    fit <- .stepJsmurf(data = data, filter = filter, family = family, q = q1, sd = sd, startTime = startTime,
                       output = "everything")
    ret <- .removeAdditionalArgsImprove(data = data, fit = fit, filter = filter, method = method,
                                        q = q2, alpha = alpha2, startTime = startTime,
                                        output = "everyGrid", ...)
  } else {
    fit <- .stepJsmurf(data = data, filter = filter, family = family, q = q1, sd = sd, startTime = startTime,
                       output = "onlyFit")
    ret <- .removeAdditionalArgsImprove(data = data, fit = fit, filter = filter, method = method,
                                        q = q2, alpha = alpha2, startTime = startTime,
                                        output = "onlyIdealization", ...)
  }
  
  q2 <- attr(ret, "q")
  attr(ret, "q") <- NULL
  if (output != "onlyIdealization") {
    if (output == "everything") {
      if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
        ret <- list(idealization = ret, fit = fit$fit, q1 = q1, q2 = q2, filter = filter, sd = sd)
      } else {
        ret <- list(idealization = ret, fit = fit$fit, q1 = q1, q2 = q2, filter = filter)
      }
    } else {
      if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
        ret <- list(idealization = ret, fit = fit, q1 = q1, q2 = q2, filter = filter, sd = sd)
      } else {
        ret <- list(idealization = ret, fit = fit, q1 = q1, q2 = q2, filter = filter)
      }
    }
  }
  
  ret
}
  