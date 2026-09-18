deconvolveLocally <- function(fit, data, filter, startTime = 0, regularization = 1,
                              thresholdLongSegment = 10L, localEstimate = stats::median,
                              gridSize = c(1, 1 / 10, 1 / 100) / filter$sr,
                              windowFactorRefinement = 1,
                              output = c("onlyIdealization", "everyGrid"), report = FALSE,
                              suppressWarningNoDeconvolution = FALSE) {
  if (!is(fit, "stepblock")) {
    if (is.list(fit) && !is.null(fit$fit) && is(fit$fit, "stepblock")) {
      fit <- fit$fit
    } else {
      stop("argument 'fit' must be an object of class 'stepblock'")
    }
  }
  
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class lowpassFilter")
  }
  
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("data must be a finite numerical vector")
  }
  
  if (!is.numeric(startTime) || length(startTime) != 1 || !is.finite(startTime)) {
    stop("startTime must be a single finite numeric")
  }
  time <- startTime + seq(along = data) / filter$sr
  
  if (!is.numeric(gridSize) || any(!is.finite(gridSize))) {
    stop("gridSize must be a finite numeric vector")
  }
  
  if (length(windowFactorRefinement) == 1) {
    windowFactorRefinement <- rep(windowFactorRefinement, length(gridSize) - 1)
  } else {
    if (length(windowFactorRefinement) != length(gridSize) - 1) {
      stop("windowFactorRefinement must be of length one or length(gridSize) - 1")
    }
  }
  
  if (!is.numeric(windowFactorRefinement) || any(!is.finite(windowFactorRefinement))) {
    stop(paste("windowFactorRefinement must be a finite numeric, ",
               "either a single value or vector of length length(gridSize) - 1", sep = ""))
  }
  
  correlations <- vector("list", length(gridSize))
  for (i in 1:length(gridSize)) {
    if (is.list(regularization)) {
      if (length(regularization) != length(gridSize)) {
        stop("regularization must be of the same length as 'gridSize' if it is a list")
      }
      
      if (!is.numeric(regularization[[i]]) || any(!is.finite(regularization[[i]]))) {
        stop("all entries of 'regularization' must be finite numerics")
      }
      
      regu <- regularization[[i]][1:(filter$len + 1)]
      regu[is.na(regu)] <- 0
      correlations[[i]] <- filter$acf + regu
    } else {
      if (!is.numeric(regularization) || any(!is.finite(regularization))) {
        stop("all entries of 'regularization' must be finite numerics")
      }
      regu <- regularization[1:(filter$len + 1)]
      regu[is.na(regu)] <- 0
      correlations[[i]] <- filter$acf + regu
    }
  }
  
  if (!is.numeric(thresholdLongSegment) || length(thresholdLongSegment) != 1 ||
      !is.finite(thresholdLongSegment)) {
    stop("thresholdLongSegment must be a single positive integer")
  }
  
  if (!is.integer(thresholdLongSegment)) {
    thresholdLongSegment <- as.integer(thresholdLongSegment + 1e-6)
  }
  
  if (thresholdLongSegment <= 0L) {
    stop("thresholdLongSegment must be a single positive integer")
  }
  
  if (!is.function(localEstimate) || length(names(formals(localEstimate))) == 0) {
    stop("localEstimate must be a function with at least one argument")
  }
  
  if (!is.logical(report) || length(report) != 1 || is.na(report)) {
    stop("report must be a single logical (not NA)")
  }
  
  if (!is.logical(suppressWarningNoDeconvolution) || length(suppressWarningNoDeconvolution) != 1 || 
      is.na(suppressWarningNoDeconvolution)) {
    stop("suppressWarningNoDeconvolution must be a single logical (not NA)")
  }
  
  output <- match.arg(output)
  
  # find long segments and reestimate their values
  shiftStart <- filter$len / filter$sr
  shiftEnd <- filter$len / filter$sr
  tolerance <- 1e-6 / filter$sr
  value <- fit$value
  for (i in seq(along = fit$leftEnd)) {
    indices <- .whichIndices(time, fit$leftEnd[i] + shiftStart - tolerance,
                                            fit$rightEnd[i] - shiftEnd + tolerance)
    
    if (length(indices) >= thresholdLongSegment) {
      est <- localEstimate(data[indices])
      if (!is.numeric(est) || length(est) != 1 || !is.finite(est)) {
        stop("localEstimate must return a single finite numeric")
      }
      value[i] <- est
    } else {
      value[i] <- NA 
    }
  }
  
  # initialize the deconvolution
  if (length(value) > 1) {
    if (output == "everyGrid") {
      deconvolution <- list()
      for (i in seq(along = gridSize)) {
        deconvolution[[i]] <- stepR::stepblock(value = value,
                                               leftEnd = numeric(length(value)),
                                               rightEnd = numeric(length(value)),
                                               x0 = attr(fit, "x0"))
        
        deconvolution[[i]]$value <- value
        deconvolution[[i]]$leftEnd[1] <- fit$leftEnd[1]
        deconvolution[[i]]$leftEnd[-1] <- NA
        deconvolution[[i]]$rightEnd[-length(value)] <- NA
        deconvolution[[i]]$rightEnd[length(value)] <- fit$rightEnd[length(value)]
        class(deconvolution[[i]]) <- c("localDeconvolution", class(deconvolution[[i]]))
      }
    } else {
      deconvolution <- stepR::stepblock(value = value,
                                        leftEnd = numeric(length(value)),
                                        rightEnd = numeric(length(value)),
                                        x0 = attr(fit, "x0"))
      
      deconvolution$value <- value
      deconvolution$leftEnd[1] <- fit$leftEnd[1]
      deconvolution$leftEnd[-1] <- NA
      deconvolution$rightEnd[-length(value)] <- NA
      deconvolution$rightEnd[length(value)] <- fit$rightEnd[length(value)]
      class(deconvolution) <- c("localDeconvolution", class(deconvolution))
    }
    noDeconvolution <- integer()
  } else {
    if (is.na(value)) {
      value <- fit$value
      noDeconvolution <- 1L
      
      if (!suppressWarningNoDeconvolution) {
        suppressWarningNoDeconvolution <- TRUE
        warning("at least one segment could not be deconvolved ",
                "since two successive short segments (or a short segment at the begin or end) occurred")
      }
    } else {
      noDeconvolution <- integer()
    }
    
    if (output == "everyGrid") {
      deconvolution <- list()
      for (i in 1:length(gridSize)) {
        
        deconvolution[[i]] <- stepR::stepblock(value = value,
                                               leftEnd = fit$leftEnd,
                                               rightEnd = fit$rightEnd,
                                               x0 = attr(fit, "x0"))
        
        class(deconvolution[[i]]) <- c("localDeconvolution", class(deconvolution[[i]]))
      }
    } else {
      deconvolution <- stepR::stepblock(value = value,
                                        leftEnd = fit$leftEnd,
                                        rightEnd = fit$rightEnd,
                                        x0 = attr(fit, "x0"))
      
      class(deconvolution) <- c("localDeconvolution", class(deconvolution))
    }
  }
  
  # deconvolution
  i <- 1L
  while (i < length(value)) {
    if (!is.na(value[i]) && !is.na(value[i + 1])) {
      # jump
      indices <- .whichIndices(time, fit$leftEnd[i + 1] - shiftEnd + tolerance,
                                              fit$rightEnd[i] + shiftStart - tolerance)
      
      if (report) {
        message("Deconvolve change ", i , " from in total ", length(value[-1]), " changes.")
      }
      
      gridIndices <- .whichIndices(time, fit$leftEnd[i + 1] - shiftEnd - tolerance, 
                                                  fit$rightEnd[i] + tolerance)
      cp <- lowpassFilter::.deconvolveJump(seq(time[gridIndices[1]], time[gridIndices[length(gridIndices)]], gridSize[1]),
                                           data[indices], time[indices],
                                           as.numeric(value[i]), as.numeric(value[i + 1]),
                                           filter$number, filter$list, correlations[[1]])
      if (output == "everyGrid") {
        deconvolution[[1]]$rightEnd[i] <- cp
        deconvolution[[1]]$leftEnd[i + 1L] <- cp
      }
      
      for (j in seq(along = gridSize)[-1]) {
        cp <- lowpassFilter::.deconvolveJump(seq(cp - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                 cp + windowFactorRefinement[j - 1] * gridSize[j - 1], gridSize[j]),
                                             data[indices], time[indices],
                                             as.numeric(value[i]), as.numeric(value[i + 1]),
                                             filter$number, filter$list, correlations[[j]])
        if (output == "everyGrid") {
          deconvolution[[j]]$rightEnd[i] <- cp
          deconvolution[[j]]$leftEnd[i + 1] <- cp
        }
      }
      
      if (output == "onlyIdealization") {
        deconvolution$rightEnd[i] <- cp
        deconvolution$leftEnd[i + 1] <- cp
      }
    } else if (!is.na(value[i]) && is.na(value[i + 1]) && i + 1 != length(value) && !is.na(value[i + 2])) {
      # isolated peak
      indices <- .whichIndices(time, fit$leftEnd[i + 1] - shiftEnd + tolerance,
                                              fit$rightEnd[i + 1] + shiftStart - tolerance)
      
      if (report) {
        message("Deconvolve changes ", i , " and ", i + 1, " from in total ", length(value[-1]), " changes.")
      }
      
      gridIndices1 <- .whichIndices(time, fit$leftEnd[i + 1] - shiftEnd - tolerance, 
                                                   fit$rightEnd[i] + tolerance)
      gridIndices2 <- .whichIndices(time, fit$leftEnd[i + 2] - shiftEnd - tolerance, 
                                                   fit$rightEnd[i + 1] + tolerance)
      
      ret <- lowpassFilter::.deconvolvePeak(seq(time[gridIndices1[1]], time[gridIndices1[length(gridIndices1)]],
                                                gridSize[1]),
                                            seq(time[gridIndices2[1]], time[gridIndices2[length(gridIndices2)]],
                                                gridSize[1]),
                                            data[indices], time[indices],
                                            as.numeric(value[i]), as.numeric(value[i + 2]),
                                            filter$number, filter$list,
                                            correlations[[1]], gridSize[1] * 1e-3)
      
      if (output == "everyGrid") {
        deconvolution[[1]]$rightEnd[i] <- ret$left
        deconvolution[[1]]$leftEnd[i + 1] <- ret$left
        deconvolution[[1]]$rightEnd[i + 1] <- ret$right
        deconvolution[[1]]$leftEnd[i + 2] <- ret$right
        deconvolution[[1]]$value[i + 1] <- ret$value
      }
      
      for (j in seq(along = gridSize)[-1]) {
        ret <- lowpassFilter::.deconvolvePeak(seq(ret$left - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                  ret$left + windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                  gridSize[j]),
                                              seq(ret$right - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                  ret$right + windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                  gridSize[j]),
                                              data[indices], time[indices],
                                              as.numeric(value[i]), as.numeric(value[i + 2]),
                                              filter$number, filter$list,
                                              correlations[[j]], gridSize[j] * 1e-3)
        
        if (output == "everyGrid") {
          deconvolution[[j]]$rightEnd[i] <- ret$left
          deconvolution[[j]]$leftEnd[i + 1] <- ret$left
          deconvolution[[j]]$rightEnd[i + 1] <- ret$right
          deconvolution[[j]]$leftEnd[i + 2] <- ret$right
          deconvolution[[j]]$value[i + 1] <- ret$value
        }
      }
      
      if (output == "onlyIdealization") {
        deconvolution$rightEnd[i] <- ret$left
        deconvolution$leftEnd[i + 1] <- ret$left
        deconvolution$rightEnd[i + 1] <- ret$right
        deconvolution$leftEnd[i + 2] <- ret$right
        deconvolution$value[i + 1] <- ret$value
      }
      
      i <- i + 1L
    } else {
      # deconvolution impossible
      if (!suppressWarningNoDeconvolution) {
        suppressWarningNoDeconvolution <- TRUE
        warning("at least one segment could not be deconvolved ",
                "since two successive short segments (or a short segment at the begin or end) occurred")
      }
      
      if (is.na(value[i])) {
        noDeconvolution[length(noDeconvolution) + 1] <- i
        
        if (i != 1L) {
          stop("unexpected result")
        }
        
        if (output == "everyGrid") {
          for (j in 1:length(gridSize)) {
            deconvolution[[j]]$leftEnd[i] <- fit$leftEnd[i]
            deconvolution[[j]]$value[i] <- fit$value[i]
            deconvolution[[j]]$rightEnd[i] <- fit$rightEnd[i]
            if (i != length(value)) {
              deconvolution[[j]]$leftEnd[i + 1] <- fit$leftEnd[i + 1]
            }
          }
        } else {
          deconvolution$leftEnd[i] <- fit$leftEnd[i]
          deconvolution$value[i] <- fit$value[i]
          deconvolution$rightEnd[i] <- fit$rightEnd[i]
          if (i != length(value)) {
            deconvolution$leftEnd[i + 1] <- fit$leftEnd[i + 1]
          }
        }
      }
      
      if (i == length(value)) {
        break
      }
      
      while (is.na(value[i + 1])) {
        i <- i + 1L
        noDeconvolution[length(noDeconvolution) + 1] <- i
        
        if (output == "everyGrid") {
          for (j in 1:length(gridSize)) {
            deconvolution[[j]]$rightEnd[i - 1] <- fit$rightEnd[i - 1]
            deconvolution[[j]]$leftEnd[i] <- fit$leftEnd[i]
            deconvolution[[j]]$value[i] <- fit$value[i]
            deconvolution[[j]]$rightEnd[i] <- fit$rightEnd[i]
            if (i != length(value)) {
              deconvolution[[j]]$leftEnd[i + 1] <- fit$leftEnd[i + 1]
            }
          }
        } else {
          deconvolution$rightEnd[i - 1] <- fit$rightEnd[i - 1]
          deconvolution$leftEnd[i] <- fit$leftEnd[i]
          deconvolution$value[i] <- fit$value[i]
          deconvolution$rightEnd[i] <- fit$rightEnd[i]
          if (i != length(value)) {
            deconvolution$leftEnd[i + 1] <- fit$leftEnd[i + 1]
          }
        }
        if (i == length(value)) {
          break
        }
      }
    }
    i <- i + 1L
  }
  
  attr(deconvolution, "noDeconvolution") <- noDeconvolution
  deconvolution
}

# indices such that time > start and time < end
# which(time > start & time < end)
.whichIndices <- function(time, start, end) {
  if (start > end) {
    return(integer(0))
  }
  
  left <- 1L
  right <- length(time)
  m <- as.integer((left + right) / 2 + 0.1)
  
  while (left != right) {
    if (time[m] > start) {
      right <- m
    } else {
      left <- m + 1
    }
    m <- as.integer((left + right) / 2 + 0.1)
  }
  
  start <- left
  
  right <- length(time)
  m <- as.integer((left + right) / 2 + 0.6)
  
  while (left != right) {
    if (time[m] < end) {
      left <- m
    } else {
      right <- m - 1
    }
    m <- as.integer((left + right) / 2 + 0.6)
  }
  
  end <- left
  if (start <= end) {
    indices <- start:end
  } else {
    indices <- integer(0)
  }
  indices
}
