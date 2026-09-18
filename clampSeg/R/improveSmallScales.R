improveSmallScales <- function(fit, data, filter, method = c("2Param", "LR"),
                               lengths = NULL, q = NULL, alpha = 0.04, r = 1e3, startTime = 0,
                               thresholdLongSegment = if (method == "LR") 10L else 25L,
                               localValue = stats::median,
                               localVar = function(data) stepR::sdrobnorm(data,
                                                                          lag = filter$len + 1L)^2,
                               regularization = 1,
                               gridSize = c(1, 1 / 10, 1 / 100) / filter$sr,
                               windowFactorRefinement = 1,
                               output = c("onlyIdealization", "everyGrid"), report = FALSE,
                               suppressWarningNoDeconvolution = FALSE,
                               localList = NULL, ...) {
  method <- match.arg(method)
  
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

  if (!is.logical(report) || length(report) != 1 || is.na(report)) {
    stop("report must be a single logical (not NA)")
  }
  
  if (!is.numeric(gridSize) || any(!is.finite(gridSize))) {
    stop("gridSize must be a finite numeric vector")
  }
  
  if (gridSize[1] != 1 / filter$sr) {
    warning("the first element of gridSize must be equal to 1 / filter$sr, it will be set to this value")
    gridSize[1] <- 1 / filter$sr
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
  
  output <- match.arg(output)
  
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
  
  if (method == "LR") {
    ret <- stepR::.testSmallScales(data = data, fit = fit, filter = filter, family = method, lengths = lengths,
                                   q = q, alpha = alpha, r = r, startTime = startTime,
                                   thresholdLongSegment = thresholdLongSegment,
                                   localValue = localValue,
                                   correlations = correlations[[1]],
                                   suppressWarningNoDeconvolution = suppressWarningNoDeconvolution,
                                   localList = localList, ...)  
  } else {
    ret <- stepR::.testSmallScales(data = data, fit = fit, filter = filter, family = method, lengths = lengths,
                                   q = q, alpha = alpha, r= r, startTime = startTime,
                                   thresholdLongSegment = thresholdLongSegment,
                                   localValue = localValue,
                                   localVar = localVar,
                                   correlations = correlations[[1]],
                                   suppressWarningNoDeconvolution = suppressWarningNoDeconvolution,
                                   localList = localList, ...)  
  }

  jumps <- ret$jumps
  addLeft <- ret$addLeft
  addRight <- ret$addRight
  noDeconvolution <- ret$noDeconvolution
  data <- ret$data
  q <- ret$q
  
  startTime <- data$startTime
  time <- data$time
  filter <- data$filter
  tolerance <- data$tolerance
  data <- data$y
  
  # initialize the deconvolution
  if (output == "everyGrid") {
    left <- list()
    right <- list()
    value <- list()
    for (i in seq(along = gridSize)) {
      left[[i]] <- startTime
      right[[i]] <- numeric()
      value[[i]] <- numeric()
    }
  } else {
    left <- startTime
    right <- numeric()
    value <- numeric()
  }
  noD <- integer(0L)
  
  startSegment <- filter$len
  index <- 1L
  indexAdd <- 1L
  indexJump <- 1L
  nextCp2 <- NA
  notDeconvolable <- FALSE
  noRepeat <- FALSE
  
  if (indexAdd <= length(addLeft) || indexJump <= length(jumps)) {
    # find end of the first segment
    if (is.na(jumps[indexJump]) || (!is.na(addLeft[indexAdd]) && addLeft[indexAdd] < jumps[indexJump])) {
      endSegment <- addLeft[indexAdd]
      nextStartSegment <- addRight[indexAdd] + filter$len
      nextCp1 <- addLeft[indexAdd]
      nextCp2 <- addRight[indexAdd]
      if (noDeconvolution[indexAdd]) {
        notDeconvolable <- TRUE
      }
      nextJump <- FALSE
      indexAdd <- indexAdd + 1L
    } else {
      endSegment <- jumps[indexJump] - 1L
      nextStartSegment <- jumps[indexJump] + 1L + filter$len
      nextCp1 <- jumps[indexJump]
      nextJump <- TRUE
      indexJump <- indexJump + 1L
    }
  } else {
    endSegment <- length(data) - filter$len
    nextStartSegment <- filter$len
    nextCp1 <- NA
    nextJump <- NA
    noRepeat <- TRUE
  }
  
  if (endSegment - startSegment + 1L < thresholdLongSegment) {
    repeat {
      startSegment <- nextStartSegment
      if (!is.na(nextJump)) {
        if (nextJump) {
          endSegmentShort <- nextCp1
        } else {
          endSegmentShort <- nextCp2
        }
      }
      
      if (indexAdd > length(addLeft) && indexJump > length(jumps)) {
        endSegment <- length(data) - filter$len
        
        if (endSegment - startSegment + 1L < thresholdLongSegment) {
          # return single not deconvovable segment
          if (!suppressWarningNoDeconvolution) {
            warning("at least one segment could not be deconvolved ",
                    "since two successive short segments (or a short segment at the begin or end) occurred")
          }
          
          if (output == "everyGrid") {
            ret <- list()
            for (j in seq(along = gridSize)) {
              ret[[j]] <- stepR::stepblock(value = localValue(data), leftEnd = startTime,
                                           rightEnd = time[length(time)], x0 = startTime)
              class(ret[[j]]) <- c("localDeconvolution", class(ret[[j]]))
            }
          } else {
            ret <- stepR::stepblock(value = localValue(data), leftEnd = startTime,
                                    rightEnd = time[length(time)], x0 = startTime)
            class(ret) <- c("localDeconvolution", class(ret))
          }
          attr(ret, "noDeconvolution") <- 1L
          attr(ret, "q") <- q
          return(ret)
        }
        
        jump <- nextJump
        cp1 <- nextCp1
        cp2 <- nextCp2
        noRepeat <- TRUE
        break
      }
      
      if (is.na(jumps[indexJump]) || (!is.na(addLeft[indexAdd]) && addLeft[indexAdd] < jumps[indexJump])) {
        endSegment <- addLeft[indexAdd]
        nextStartSegment <- addRight[indexAdd] + filter$len
        nextCp1 <- addLeft[indexAdd]
        nextCp2 <- addRight[indexAdd]
        nextJump <- FALSE
        indexAdd <- indexAdd + 1L
      } else {
        endSegment <- jumps[indexJump] - 1L
        nextStartSegment <- jumps[indexJump] + 1L + filter$len
        nextCp1 <- jumps[indexJump]
        nextJump <- TRUE
        indexJump <- indexJump + 1L
      }
      
      if (endSegment - startSegment + 1L >= thresholdLongSegment) {
        break
      }
    }
    
    # enter first short segment in output
    if (output == "everyGrid") {
      for (i in seq(along = gridSize)) {
        left[[i]][2] <- time[endSegmentShort]
        right[[i]][1] <- time[endSegmentShort]
        value[[i]][1] <- localValue(data[1L:endSegmentShort])
      }
    } else {
      left[2] <- time[endSegmentShort]
      right[1] <- time[endSegmentShort]
      value[1] <- localValue(data[1L:endSegmentShort])
    }
    
    noD <- 1L
    if (!suppressWarningNoDeconvolution) {
      suppressWarningNoDeconvolution <- TRUE
      warning("at least one segment could not be deconvolved ",
              "since two successive short segments (or a short segment at the begin or end) occurred")
    }
    
    index <- 2L
  }
  
  if (output == "everyGrid") {
    for (i in seq(along = gridSize)) {
      value[[i]][index] <- localValue(data[startSegment:endSegment])
    }
  } else {
    value[index] <- localValue(data[startSegment:endSegment])
  }
  
  # initilize 
  startSegment <- nextStartSegment
  jump <- nextJump
  cp1 <- nextCp1
  cp2 <- nextCp2
  nextNotDeconvolable <- FALSE
  reachedEnd <- FALSE
  
  if (!noRepeat) {
    repeat {
      # find next long segment
      repeat {
        if (indexAdd > length(addLeft) && indexJump > length(jumps)) {
          reachedEnd <- TRUE
          endSegment <- length(data) - filter$len
          
          if (endSegment - startSegment + 1L < thresholdLongSegment) {
            # return with last segment not deconvolable
            start <- cp1 + filter$len
            
            if (output == "everyGrid") {
              ret <- list()
              for (i in seq(along = gridSize)) {
                left[[i]][index + 1L] <- time[start]
                right[[i]][index] <- time[start]
                value[[i]][index + 1L] <- localValue(data[(start + 1L):length(data)])
                right[[i]][length(right[[i]]) + 1L] <- time[length(time)]
                ret[[i]] <- stepR::stepblock(value = value[[i]], leftEnd = left[[i]],
                                             rightEnd = right[[i]], x0 = startTime)
                class(ret[[i]]) <- c("localDeconvolution", class(ret[[i]]))
              }
            } else {
              left[index + 1L] <- time[start]
              right[index] <- time[start]
              value[index + 1L] <- localValue(data[(start + 1L):length(data)])
              right[length(right) + 1L] <- time[length(time)]
              ret <- stepR::stepblock(value = value, leftEnd = left, rightEnd = right, x0 = startTime)
              class(ret) <- c("localDeconvolution", class(ret))
            }
            
            noD[length(noD) + 1] <- index + 1L
            attr(ret, "noDeconvolution") <- unique(noD)
            
            if (!suppressWarningNoDeconvolution) {
              suppressWarningNoDeconvolution <- TRUE
              warning("at least one segment could not be deconvolved ",
                      "since two successive short segments (or a short segment at the begin or end) occurred")
            }
            
            attr(ret, "q") <- q
            return(ret)
          }
          
          break
        }
        
        if (is.na(jumps[indexJump]) || (!is.na(addLeft[indexAdd]) && addLeft[indexAdd] < jumps[indexJump])) {
          endSegment <- addLeft[indexAdd]
          nextStartSegment <- addRight[indexAdd] + filter$len
          nextCp1 <- addLeft[indexAdd]
          nextCp2 <- addRight[indexAdd]
          if (noDeconvolution[indexAdd]) {
            nextNotDeconvolable <- TRUE
          }
          nextJump <- FALSE
          indexAdd <- indexAdd + 1L
        } else {
          endSegment <- jumps[indexJump] - 1L
          nextStartSegment <- jumps[indexJump] + 1L + filter$len
          nextCp1 <- jumps[indexJump]
          nextJump <- TRUE
          indexJump <- indexJump + 1L
        }
        
        if (endSegment - startSegment + 1L >= thresholdLongSegment) {
          break
        }
        
        startSegment <- nextStartSegment
        jump <- FALSE
        notDeconvolable <- TRUE
        if (nextJump) {
          cp2 <- nextCp1
        } else {
          cp2 <- nextCp2
        }
      }
      
      if (jump) {
        # deconvolution jump
        cp <- time[cp1]
        
        if (report) {
          message(index, ": jump at ", cp, ".")
        }
        
        rightValue <- localValue(data[startSegment:endSegment])
        if (output == "everyGrid") {
          value[[1]][index + 1L] <- rightValue
          right[[1]][index] <- cp
          left[[1]][index + 1L] <- cp
          leftValue <- value[[1]][index]
        } else {
          leftValue <- value[index]
        }
        
        indices <- .whichIndices(time, cp - tolerance, cp + filter$len / filter$sr + tolerance)
        for (j in seq(along = gridSize)[-1]) {
          cp <- lowpassFilter::.deconvolveJump(seq(cp - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                   cp + windowFactorRefinement[j - 1] * gridSize[j - 1], gridSize[j]),
                                               data[indices], time[indices],
                                               as.numeric(leftValue), as.numeric(rightValue),
                                               filter$number, filter$list, correlations[[j]])
          if (output == "everyGrid") {
            value[[j]][index + 1L] <- rightValue
            right[[j]][index] <- cp
            left[[j]][index + 1L] <- cp
          }
        }
        
        if (output == "onlyIdealization") {
          value[index + 1L] <- rightValue
          right[index] <- cp
          left[index + 1L] <- cp
        }
        
        index <- index + 1L
      } else {
        # deconvolution peak
        
        if (report) {
          message(index, ": peak at ", time[cp1], " and ", time[cp2], ".")
        }
        
        rightValue <- localValue(data[startSegment:endSegment])
        if (output == "everyGrid") {
          leftValue <- value[[1]][index]
        } else {
          leftValue <- value[index]
        }
        
        indices <- (cp1 + 1L):(cp2 + filter$len - 1L)
        ret <- lowpassFilter::.deconvolvePeak(time[cp1:(cp1 + filter$len)], time[(cp2 - filter$len):cp2],
                                              data[indices], time[indices],
                                              leftValue, rightValue,
                                              filter$number, filter$list,
                                              correlations[[1]], gridSize[1] * 1e-3)
        
        if (output == "everyGrid") {
          right[[1]][index] <- ret$left
          left[[1]][index + 1L] <- ret$left
          right[[1]][index + 1L] <- ret$right
          left[[1]][index + 2L] <- ret$right
          value[[1]][index + 1L] <- ret$value
          value[[1]][index + 2L] <- rightValue
        }
        
        for (j in seq(along = gridSize)[-1]) {
          ret <- lowpassFilter::.deconvolvePeak(seq(ret$left - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                    ret$left + windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                    gridSize[j]),
                                                seq(ret$right - windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                    ret$right + windowFactorRefinement[j - 1] * gridSize[j - 1],
                                                    gridSize[j]),
                                                data[indices], time[indices],
                                                leftValue, rightValue,
                                                filter$number, filter$list,
                                                correlations[[j]], gridSize[j] * 1e-3)
          
          if (output == "everyGrid") {
            right[[j]][index] <- ret$left
            left[[j]][index + 1L] <- ret$left
            right[[j]][index + 1L] <- ret$right
            left[[j]][index + 2L] <- ret$right
            value[[j]][index + 1L] <- ret$value
            value[[j]][index + 2L] <- rightValue
          }
        }
        
        if (output == "onlyIdealization") {
          right[index] <- ret$left
          left[index + 1L] <- ret$left
          right[index + 1L] <- ret$right
          left[index + 2L] <- ret$right
          value[index + 1L] <- ret$value
          value[index + 2L] <- rightValue
        }
        
        if (notDeconvolable) {
          # noDeconvolution
          noD[length(noD) + 1] <- index + 1L
          
          if (!suppressWarningNoDeconvolution) {
            suppressWarningNoDeconvolution <- TRUE
            warning("at least one segment could not be deconvolved ",
                    "since two successive short segments (or a short segment at the begin or end) occurred")
          }
        }
        index <- index + 2L
      }
      
      if (reachedEnd) {
        break
      }
      
      startSegment <- nextStartSegment
      jump <- nextJump
      nextNotDeconvolable <- FALSE
      notDeconvolable <- nextNotDeconvolable
      cp1 <- nextCp1
      cp2 <- nextCp2
    }
  }
  
  if (output == "everyGrid") {
    ret <- list()
    for (j in seq(along = gridSize)) {
      right[[j]][length(right[[j]]) + 1L] <- time[length(time)]
      ret[[j]] <- stepR::stepblock(value = value[[j]], leftEnd = left[[j]],
                                   rightEnd = right[[j]], x0 = startTime)
      class(ret[[j]]) <- c("localDeconvolution", class(ret[[j]]))
    }
  } else {
    right[length(right) + 1L] <- time[length(time)]
    ret <- stepR::stepblock(value = value, leftEnd = left, rightEnd = right, x0 = startTime)
    class(ret) <- c("localDeconvolution", class(ret))
  }
  attr(ret, "noDeconvolution") <- unique(noD)
  attr(ret, "q") <- q
  ret
}
