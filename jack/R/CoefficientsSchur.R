#' @importFrom multicool multinom
#' @importFrom gmp as.bigq asNumeric
#' @importFrom partitions parts
NULL

SchurCoefficientsQ <- function(n, until = NULL){
  stopifnot(n > 0L, isPositiveInteger(n))
  if(n == 1L) {
    out <- as.matrix("1")
    rownames(out) <- colnames(out) <- "1"
    return(out)
  }
  allParts <- parts(n) #dominatedPartitions(n)
  nParts <- ncol(allParts)
  stringParts <- apply(allParts, 2L, toString)
  if(!is.null(until)){
    until <- toString(c(until, rep(0, n-length(until))))
    lastRow <- match(until, stringParts)
  }else{
    lastRow <- nParts
  }
  indices <- matrix(seq_len(lastRow*nParts),
                    nrow = lastRow, ncol = nParts)
  colnames(indices) <- stringParts
  rownames(indices) <- stringParts[1L:lastRow]
  coefs <- as.bigq(integer(lastRow*nParts))
  coefs[1L] <- as.bigq(1L)
  for(m in 1L:min(lastRow, nParts-1L)){
    kappa <- allParts[,m]
    for(k in (m+1L):nParts){
      lambda <- allParts[,k]
      # btwn <- betweenPartitions(lambda, kappa)
      x <- as.bigq(0L)
      for(i in 1L:(n-1L)){
        for(j in (i+1L):n){
          for(t in seq_len(lambda[j])){
            mu <- as.bigq(lambda)
            mu[i] <- mu[i] + as.bigq(t)
            mu[j] <- mu[j] - as.bigq(t)
            muOrd <- sort(asNumeric(mu), decreasing = TRUE)
            if(isDominated(muOrd, kappa)){
              x <- x + (mu[i]-mu[j]) /
                (.e(kappa, as.bigq(1L))-.e(lambda, as.bigq(1L))) *
                coefs[indices[m, toString(muOrd)]]
            }
          }
        }
      }
      coefs[indices[m, toString(lambda)]] <- x
    }
    if(m < lastRow){
      coefs[indices[m+1L,m+1L]] <- as.bigq(1L)
    }
  }
  out <- matrix(as.character(coefs), nrow = lastRow, ncol = nParts)
  dimnames(out) <- dimnames(indices)
  out
  # .coefs <- as.bigq(matrix(0L, nrow = lastRow, ncol = nParts))
  # for(i in 1L:lastRow){
  #   for(j in i:nParts){
  #     .coefs[i,j] <- coefs[indices[i,j]]
  #   }
  # }
  # lastColumn <- .coefs[, nParts]
  # facto <- as.bigq(factorialZ(n))
  # for(k in 1L:lastRow){
  #   lambda <- allParts[,k]
  #   i <- rep(seq_along(lambda), times = lambda)
  #   j <- unlist(sapply(lambda, seq_len, simplify = FALSE))
  #   lambdaPrime <- dualPartition(lambda)
  #   hookslengths <- lambdaPrime[j] - i + lambda[i] - j + 1
  #   f <- facto / lastColumn[k]
  #   coefs[k,] <- f * .coefs[k,] / prod(as.bigq(hookslengths))
  # }
  # out <- as.character(.coefs)
  # dimnames(out) <- dimnames(indices)
  # out
}

SchurCoefficientsNum <- function(n, until = NULL){
  stopifnot(n > 0L, isPositiveInteger(n))
  if(n == 1L) {
    out <- as.matrix(1)
    rownames(out) <- colnames(out) <- "1"
    return(out)
  }
  allParts <- parts(n) #dominatedPartitions(n)
  nParts <- ncol(allParts)
  stringParts <- apply(allParts, 2L, toString)
  if(!is.null(until)){
    until <- toString(c(until, rep(0, n-length(until))))
    lastRow <- match(until, stringParts)
  }else{
    lastRow <- nParts
  }
  coefs <- matrix(0, nrow = lastRow, ncol = nParts)
  colnames(coefs) <- stringParts
  rownames(coefs) <- stringParts[1L:lastRow]
  coefs[1L,1L] <- 1
  for(m in 1L:min(lastRow, nParts-1L)){
    kappa <- allParts[,m]
    for(k in (m+1L):nParts){
      lambda <- allParts[,k]
      # btwn <- betweenPartitions(lambda, kappa)
      x <- 0
      for(i in 1L:(n-1L)){
        for(j in (i+1L):n){
          for(t in seq_len(lambda[j])){
            mu <- lambda
            mu[i] <- mu[i] + t
            mu[j] <- mu[j] - t
            muOrd <- sort(mu, decreasing = TRUE)
            if(isDominated(muOrd, kappa)){
              x <- x + (mu[i]-mu[j]) /
                (.e(kappa, 1)-.e(lambda, 1)) *
                coefs[m, toString(muOrd)]
            }
          }
        }
      }
      coefs[m, toString(lambda)] <- x
    }
    if(m < lastRow){
      coefs[m+1L,m+1L] <- 1
    }
  }
  coefs
}

SchurCoefficients <- function(n, until = NULL, exact = FALSE){
  if(exact){
    SchurCoefficientsQ(n, until)
  }else{
    SchurCoefficientsNum(n, until)
  }
}
