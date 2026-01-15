#' @importFrom multicool multinom
#' @importFrom gmp as.bigq asNumeric
NULL

zonalQCoefficientsQ <- function(n, until = NULL){
  stopifnot(n > 0L, isPositiveInteger(n))
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
      btwn <- betweenPartitions(lambda, kappa)
      x <- as.bigq(0L)
      for(i in 1L:(n-1L)){
        for(j in (i+1L):n){
          for(t in seq_len(lambda[j])){
            mu <- as.bigq(lambda)
            mu[i] <- mu[i] + as.bigq(t)
            mu[j] <- mu[j] - as.bigq(t)
            muOrd <- sort(asNumeric(mu), decreasing = TRUE)
            if(isDominated(muOrd, kappa)){
              x <- x + 4*(mu[i]-mu[j]) /
                (.rhoQ(as.bigq(kappa))-.rhoQ(as.bigq(lambda))) *
                coefs[indices[m, toString(muOrd)]]
            }
          }
        }
      }
      coefs[indices[m, toString(lambda)]] <- x
    }
    if(m < lastRow){
      coefs[indices[m+1L,m+1L]] <-
        as.bigq(multicool::multinom(allParts[,m+1L], counts = TRUE)) -
        sum(coefs[indices[seq_len(m),m+1L]])
    }
  }
  coefs <- matrix(as.character(coefs), nrow = lastRow, ncol = nParts)
  dimnames(coefs) <- dimnames(indices)
  coefs
}

zonalQCoefficientsNum <- function(n, until = NULL){
  stopifnot(n > 0L, isPositiveInteger(n))
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
      btwn <- betweenPartitions(lambda, kappa)
      x <- 0
      for(i in 1L:(n-1L)){
        for(j in (i+1L):n){
          for(t in seq_len(lambda[j])){
            mu <- lambda
            mu[i] <- mu[i] + t
            mu[j] <- mu[j] - t
            muOrd <- sort(mu, decreasing = TRUE)
            if(isDominated(muOrd, kappa)){
              x <- x + 4*(mu[i]-mu[j]) / (.rhoQ(kappa)-.rhoQ(lambda)) *
                coefs[m, toString(muOrd)]
            }
          }
        }
      }
      coefs[m, toString(lambda)] <- x
    }
    if(m < lastRow){
      coefs[m+1L,m+1L] <-
        multicool::multinom(allParts[,m+1L], counts = TRUE) -
        sum(coefs[seq_len(m),m+1L])
    }
  }
  coefs
}

zonalQCoefficients <- function(n, until = NULL, exact = FALSE){
  if(exact){
    zonalQCoefficientsQ(n, until)
  }else{
    zonalQCoefficientsNum(n, until)
  }
}
