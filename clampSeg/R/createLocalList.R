createLocalList <- function(filter, method = c("2Param", "LR"),
                            lengths = if (method == "LR") 1:20 else 1:65) {
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class 'lowpassFilter'")
  }
  
  method <- match.arg(method)
  
  if (!is.numeric(lengths) || any(!is.finite(lengths))  || any(lengths < 1)) {
    stop("lengths must be an integer vector containing finite positive values")
  }
  
  if (any(!is.integer(lengths))) {
    lengths <- as.integer(lengths + 1e-6)
  }
  
  if (is.unsorted(lengths, strictly = TRUE)) {
    lengths <- sort(lengths)
    if (is.unsorted(lengths, strictly = TRUE)) {
      warning("lengths contains duplicated values, they will be removed")
      lengths <- unique(lengths)
    }
  }
  
  
  localList <- list()
  
  if (method == "2Param") {
    for (indexLen in seq(along = lengths)) {
      len <- lengths[indexLen]
      time <- 1:(len + filter$len - 1) / filter$sr
      cpLeft <- 0
      cpRight <- len / filter$sr
      
      Fleft <- filter$truncatedStepfun(time - cpLeft)
      Fright <- filter$truncatedStepfun(time - cpRight)
      v <- Fleft - Fright
      sumv2 <- sum(v^2)
      
      Fleft <- outer(time, time, function(i, j) filter$acAntiderivative(pmin(i, j) - cpLeft, abs(j - i)))
      Fright <- outer(time, time, function(i, j) filter$acAntiderivative(pmin(i, j) - cpRight, abs(j - i)))
      cor <- outer(time, time, function(i, j) filter$acfun(abs(j - i)))
      w <- Fleft - Fright
      sigmaL <- (cor - Fleft)
      sigmaR <- Fright
      vv <- outer(seq(along = time), seq(along = time), function(i, j) v[i] * v[j] / sum(v^2))
      diagW <- diag(w)
      matrixDiagW <- matrix(rep(diagW, length(diagW)), length(diagW))
      AL <- sum(diag(sigmaL) * diagW) - sum(vv * sigmaL * matrixDiagW)
      AR <- sum(diag(sigmaR) * diagW) - sum(vv * sigmaR * matrixDiagW)
      B <- sum(diagW^2) - sum(vv * w * matrixDiagW)
      
      w <- diagW
      sigmaL <- diag(sigmaL)
      sigmaR <- diag(sigmaR)
      
      Fleft <- 1 - filter$truncatedStepfun(time - cpLeft)
      Fright <- filter$truncatedStepfun(time - cpRight)
      
      localList[[indexLen]] = list(len = len, Fleft = Fleft, Fright = Fright, v = v, sumv2 = sumv2,
                                   sumSigmaL = AL, sumSigmaR = AR, sumW = B, w = w,
                                   sigmaL = sigmaL, sigmaR = sigmaR)
    }
    
    class(localList) <- c("localList", class(localList))
    attr(localList, "method") <- method
    attr(localList, "filter") <- filter
    attr(localList, "lengths") <- lengths
  } else {
    correlations <- filter$acf
    correlations[1] <- correlations[1] + 1
    
    for (indexLen in seq(along = lengths)) {
      len <- lengths[indexLen]
      time <- 1:(len + filter$len - 1) / filter$sr
      cpLeft <- 0
      cpRight <- len / filter$sr
      
      m <- min(len + filter$len - 1, length(correlations) - 1L)
      
      A <- matrix(0, len + filter$len - 1, len + filter$len - 1)
      for (i in 1:(len + filter$len - 2)) {
        A[i, i] <- correlations[1]
        A[i, i + 1:min(m, len + filter$len - 1 - i)] <- correlations[2:min(m + 1, len + filter$len - 1 - i + 1)]
        A[i + 1:min(m, len + filter$len - 1 - i), i] <- correlations[2:min(m + 1, len + filter$len - 1 - i + 1)]
      }
      A[len + filter$len - 1, len + filter$len - 1] <- correlations[1]
      
      Fleft <- filter$truncatedStepfun(time - cpLeft)
      Fright <- filter$truncatedStepfun(time - cpRight)
      v <- Fleft - Fright
      sol <- solve(A, v)
      vtAv <- sum(v * sol)
      
      Fleft <- 1 - Fleft
      
      localList[[indexLen]] = list(len = len, Fleft = Fleft, Fright = Fright, v = v, sol = sol, vtAv = vtAv)
    }
    
    class(localList) <- c("localList", class(localList))
    attr(localList, "method") <- method
    attr(localList, "filter") <- filter
    attr(localList, "lengths") <- lengths
  }
  
  localList
}
