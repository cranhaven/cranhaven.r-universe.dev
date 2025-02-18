# Authors: Mauro Bernardi 
#          Department Statistical Sciences
#       	 University of Padova
#      	   Via Cesare Battisti, 241
#      	   35121 PADOVA, Italy
#          E-mail: mauro.bernardi@unipd.it  

# Last change: July 20, 2021


# ::::::::::::::::::::::::::::::::::::::::
# Check functions
check_data_matrix <- function(A) {
  cond1 = (is.matrix(A))  # matrix
  cond2 = (!(any(is.infinite(A))||any(is.na(A))))
  if (cond1 && cond2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_data_vector <- function(b) {
  cond1 = ((is.vector(b))||((is.matrix(b))&&
                              (length(b)==nrow(b))||(length(b)==ncol(b))))
  cond2 = (!(any(is.infinite(b))||any(is.na(b))))
  if (cond1&&cond2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_data_positive_vector <- function(b) {
  cond1 = ((is.vector(b))||((is.matrix(b))&&
                              (length(b)==nrow(b))||(length(b)==ncol(b))))
  cond2 = (!(any(is.infinite(b))||any(is.na(b))))
  cond3 = (!(any(b<=0)))
  if (cond1&&cond2&&cond3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_param_constant <- function(num, lowerbound=0){
  cond1 = (length(num)==1)
  cond2 = ((!is.infinite(num))&&(!is.na(num)))
  cond3 = (num > lowerbound)
  
  if (cond1&&cond2&&cond3){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

check_param_constant_multiple <- function(numvec, lowerbound=0){
  for (i in 1:length(numvec)){
    if (!check_param_constant(numvec[i], lowerbound)){
      return(FALSE)
    }
  }
  return(TRUE)
}

check_param_integer <- function(num, lowerbound=0){
  cond1 = (length(num)==1)
  cond2 = ((!is.infinite(num))&&(!is.na(num)))
  cond3 = (num > lowerbound)
  cond4 = (abs(num-round(num)) < sqrt(.Machine$double.eps))
  
  if (cond1&&cond2&&cond3&&cond4){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# ::::::::::::::::::::::::::::::::::::::::
# AUXILIARY COMPUTATIONS
logseq <- function(from = 1, to = 1000,
                   length.out = 6, reverse = FALSE) {
  
  # :::::::::::::::::::::::::::::::::::::::::::::::::
  # logarithmic spaced sequence
  out <- exp(seq(log(from), log(to), length.out = length.out))
  
  # :::::::::::::::::::::::::::::::::::::::::::::::::
  # reverse the sequence
  if (reverse == TRUE) {
    res <- out[length(out):1]
  } else {
    res <- out
  }
  
  # :::::::::::::::::::::::::::::::::::::::::::::::::
  # Get output
  return(res)
}

scaledata <- function(x) {
  if (is.vector(x) == TRUE) {
    x.scaled <- (x - mean(x)) / sd(x)
  } else {
    x.scaled <- apply(x, 2, function(x) (x-mean(x))/sd(x))
  }
  return(x.scaled)
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Standardise response and design matrix
standardizemat <- function(X, y) {
  
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Get dimensions
  n <- dim(X)[1]
  
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Get std
  y  <- matrix(y, nrow = n, ncol = 1)
  mU <- diag(sqrt(diag(var(X))))
  if (ncol(y) > 1) {
    mV <- diag(sqrt(diag(var(y))))
  } else {
    mV <- sqrt(var(y))
  }
  y.ctr <- apply(y, 2, function(x) x - mean(x))
  y.std <- y.ctr %*% solve(mV)
  X.ctr <- apply(X, 2, function(x) x - mean(x))
  X.std <- X.ctr %*% solve(mU)
  
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Get output
  out       <- NULL
  out$X.std <- X.std
  out$y.std <- y.std
  out$mU    <- mU
  out$mV    <- mV
  
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Return output
  return(out)
}

# return the regression parameters in the original scale
# after standardization
invstdparms <- function(parms, mU, mV) {

  # get dimensions
  n <- dim(parms)[1]
  p <- dim(parms)[2]
  
  # standardize data
  if (p == 1) {
    parms.std <- solve(mU) %*% parms[1,] %*% mV
  } else {
    #parms.std <- t(apply(t(parms), 2, function(x) solve(mU) %*% x %*% mV))
    parms.std <- t(apply(parms, 1, function(x) solve(mU) %*% x %*% mV))
  }
  
  # convert to a mtrix object
  parms.std <- matrix(data = parms.std, nrow = n, ncol = p)
  
  # return output
  return(parms.std)
}

groups.cv <- function(n, k = 10) {
  if (k == 0) {
    k <- n
  }
  if (!is.numeric(k) || k < 0 || k > n) {
    stop("Invalid values of 'k'. Must be between 0 (for leave-one-out CV) and 'n'.")
  }
  dummyorder <- sample(1:n, size = n)
  f          <- ceiling(seq_along(dummyorder)/(n/k))
  
  groups     <- vector("list", length = max(f))
  groups.all <- NULL
  for (it in 1:max(f)) {
    indi         <- which(f == it)
    groups[[it]] <- dummyorder[indi]
    groups.all   <- c(groups.all, dummyorder[indi])
  }
  
  # groups cv
  groups.cv <- vector("list", length = max(f))
  for (it in 1:max(f)) {
    groups.cv[[it]] <- setdiff(groups.all, groups[[it]])
  }
  
  # get output
  ret             <- NULL
  ret$groups.pred <- groups
  ret$groups.cv   <- groups.cv
  ret$shuffle     <- groups.all
  
  # return output
  return(ret)
}

scaledata <- function(x) {
  if (is.vector(x) == TRUE) {
    x.scaled <- (x - mean(x)) / sd(x)
  } else {
    x.scaled <- apply(x, 2, function(x) (x-mean(x))/sd(x))
  }
  return(x.scaled)
}






