#' @importFrom gmp as.bigq is.bigq
NULL

JackEvalNaive <- function(x, lambda, alpha){
  stopifnot(isPartition(lambda))
  gmp <- is.bigq(x)
  lambda <- as.integer(lambda)
  if(length(lambda) == 0L || all(lambda == 0L)){
    return(if(gmp) as.bigq(1L) else 1)
  }
  if(gmp){
    stopifnot(is.bigq(x), is.bigq(alpha))
  }
  mus <- dominatedPartitions(lambda)
  lambda <- mus[,1L] # to add trailing zeros
  coefs <- JackCoefficients(sum(lambda), until = lambda, alpha)
  if(gmp){
    out <- as.bigq(0L)
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        as.bigq(coefs[toString(lambda), toString(mus[,i])])
    }
  }else{
    out <- 0
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        coefs[toString(lambda), toString(mus[,i])]
    }
  }
  out
}

ZonalEvalNaive <- function(x, lambda){
  stopifnot(isPartition(lambda))
  gmp <- is.bigq(x)
  lambda <- as.integer(lambda)
  if(length(lambda) == 0L || all(lambda == 0L)){
    return(if(gmp) as.bigq(1L) else 1)
  }
  mus <- dominatedPartitions(lambda)
  lambda <- mus[,1L] # to add trailing zeros
  coefs <- zonalCoefficients(sum(lambda), until = lambda, exact = gmp)
  if(gmp){
    out <- as.bigq(0L)
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        as.bigq(coefs[toString(lambda), toString(mus[,i])])
    }
  }else{
    out <- 0
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        coefs[toString(lambda), toString(mus[,i])]
    }
  }
  out
}

SchurEvalNaive <- function(x, lambda){
  stopifnot(isPartition(lambda))
  gmp <- is.bigq(x)
  lambda <- as.integer(lambda)
  if(length(lambda) == 0L || all(lambda == 0L)){
    return(if(gmp) as.bigq(1L) else 1)
  }
  mus <- dominatedPartitions(lambda)
  lambda <- mus[,1L] # to add trailing zeros
  coefs <- SchurCoefficients(sum(lambda), until = lambda, exact = gmp)
  if(gmp){
    out <- as.bigq(0L)
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        as.bigq(coefs[toString(lambda), toString(mus[,i])])
    }
  }else{
    out <- 0
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        coefs[toString(lambda), toString(mus[,i])]
    }
  }
  out
}

ZonalQEvalNaive <- function(x, lambda){
  stopifnot(isPartition(lambda))
  gmp <- is.bigq(x)
  lambda <- as.integer(lambda)
  if(length(lambda) == 0L || all(lambda == 0L)){
    return(if(gmp) as.bigq(1L) else 1)
  }
  mus <- dominatedPartitions(lambda)
  lambda <- mus[,1L] # to add trailing zeros
  coefs <- zonalQCoefficients(sum(lambda), until = lambda, exact = gmp)
  if(gmp){
    out <- as.bigq(0L)
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        as.bigq(coefs[toString(lambda), toString(mus[,i])])
    }
  }else{
    out <- 0
    for(i in 1L:ncol(mus)){
      out <- out + MSF(x, mus[,i]) *
        coefs[toString(lambda), toString(mus[,i])]
    }
  }
  out
}
