stepDetection <- function(data, filter, q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
                          output = c("onlyFit", "everything"), ...) {
  if (!is.numeric(data) || !all(is.finite(data))) {
    stop("data must be a finite numerical vector")
  }
  
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class 'lowpassFilter'")
  }
  
  if (!is.numeric(startTime) || length(startTime) != 1 || !is.finite(startTime)) {
    stop("startTime must be a single finite numeric")
  }
  time <- startTime + seq(along = data) / filter$sr
  
  if (is.null(sd)) {
    sd <- stepR::sdrobnorm(data, lag = filter$len + 1L)
  } else {
    if (!is.numeric(sd) || length(sd) != 1 || !is.finite(sd) || sd <= 0) {
      stop("sd must be a single positive finite numeric")
    }
  }
  
  if (is.null(q)) {
    if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
      stop("alpha must be a probability, i.e. a single finite numeric between 0 and 1")
    }
    
    q <- getCritVal(alpha = alpha, n = length(data), filter = filter, ...)
  } else {
    if (!is.numeric(q) || length(q) != 1 || !is.finite(q)) {
      stop("q must be a single finite numeric")
    }
  }
  
  output <- match.arg(output)
  
  fit <- stepR::stepFit(y = data, x = time, x0 = startTime, family = "mDependentPS", intervalSystem = "dyaLen",
                        q = q, covariances = sd^2 * filter$acf)
  fit <- stepR::stepblock(value = fit$value, leftEnd = c(startTime, fit$rightEnd[-length(fit$rightEnd)]),
                          rightEnd = fit$rightEnd, x0 = attr(fit, "x0"))
  
  ret <- .postFilter(fit, threshold = (filter$len - 1e-6)  / filter$sr)
  
  if (output == "everything") {
    ret = list(fit = ret, stepfit = fit, q = q, filter = filter, sd = sd)
  }
  
  ret
}

.postFilter <- function(fit, threshold) {
  leftEnd <- fit$leftEnd
  rightEnd <- fit$rightEnd
  value <- fit$value
  
  i <- 1
  while (i < length(value)) {
    j <- i
    
    decis <- .decision(i = i, j = j, leftEnd = leftEnd, value = value, threshold = threshold)
    
    while (decis) {
      j <- j + 1
      
      if (j == length(value)) {
        break
      }
      decis <- .decision(i = i, j = j, leftEnd = leftEnd, value = value, threshold = threshold)
    }
    
    if (i != j) {
      restimatedValue <- value[j]
      
      if (j < length(leftEnd)) {
        leftEnd <- c(leftEnd[1:i], leftEnd[(j + 1):length(leftEnd)])
      } else {
        leftEnd <- leftEnd[1:i]
      }
      if (i > 1) {
        rightEnd <- c(rightEnd[1:(i - 1)], rightEnd[j:length(rightEnd)])
      } else {
        rightEnd <- rightEnd[j:length(rightEnd)]
      }
      
      if (i > 1) {
        if (j < length(value)) {
          value <- c(value[1:(i - 1)], restimatedValue, value[(j + 1):length(value)])
        } else {
          value <- c(value[1:(i - 1)], restimatedValue)
        }
      } else {
        if (j < length(value)) {
          value <- c(restimatedValue, value[(j + 1):length(value)])
        } else {
          value <- restimatedValue
        }
      }
    }
    i <- i + 1
  }
  
  stepR::stepblock(value = value, leftEnd = leftEnd, rightEnd = rightEnd, x0 = attr(fit, "x0"))
}

.decision <- function(i, j, leftEnd, value, threshold) {
  if (i == 1) {
    return(FALSE)
  }
  
  leftEnd[j + 1] - leftEnd[i] < threshold && (value[j + 1] - value[j]) * (value[j] - value[j - 1]) > 0
}
