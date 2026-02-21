CalculateSignatureBurdens <- function(L, W, H) {

  # Check for errors with inputs
  check_L_W_H(L, W, H) 

  N   <- ncol(H)

  # Initialize return matrix
  ret <- sigExp_setReturn(L, W, H)

  if (!is.matrix(L)) L <- as.matrix(L)
  if (!is.matrix(W)) W <- as.matrix(W)
  H <- t(as.matrix(H))

  for(n in 1:N){
    tmp      <- L[, n, drop=FALSE] %*% H[n, , drop=FALSE]
    ret[, n] <- colSums(tmp*W)
  }

  ret

}

sigExp_setReturn <- function(L, W, H) {

  # Returns a matrix of NAs with appropriate row and column names

  N   <- ncol(H)
  K   <- ncol(W)
  ret <- matrix(data=NA, nrow=K, ncol=N)

  nms <- colnames(L)
  if (is.null(nms)) nms <- colnames(H) 
  if (is.null(nms)) nms <- paste0("Sample", 1:N)
  colnames(ret) <- nms

  nms <- colnames(W)
  if (is.null(nms)) nms <- paste0("deNovo", LETTERS[1:K])
  rownames(ret) <- nms
  
  ret

}

EstimateSigActivity <- function(V, L, W, n.start=50, iter.max=5000, eps=1e-5) {

  # Check for errors with inputs
  check_L_W_V(L, W, V)
  check_number(n.start, "n.start", min=1)
  check_number(iter.max, "iter.max", min=1)
  check_number(eps, "eps", pos=TRUE)

  op  <- list(n.start=n.start, iter.max=iter.max, eps=eps, print=0)

  # Call main function
  ret <- estSigAct_main(V, L, W, op) 
  ret
}

estSigAct_main <- function(V, L, W, op) {

  DEBUG <- 0
  if (DEBUG) {
    print(paste0("nrow(V)=", nrow(V), ", ncol(V)=", ncol(V)))
    print(paste0("nrow(L)=", nrow(L), ", ncol(L)=", ncol(L)))
    print(paste0("nrow(W)=", nrow(W), ", ncol(W)=", ncol(W)))
  }
  n     <- ncol(V)
  k     <- ncol(W)
  p     <- nrow(V)

  # Integer arguments passed into C code
  iargs <- c(n, k, p, op$n.start, op$iter.max, op$print, DEBUG)

  L[L == 0] <- 1
  V[L == 0] <- 0
  lower     <- 1e-6
  upper     <- sum(V)/sum(L)

  # Double arguments passed to C code
  dargs     <- c(op$eps, lower, upper)

  # Initialize return objects from C code
  ret_H     <- rep(-9999.0e200, k*n)
  ret_ll    <- -9999.0
  ret_conv  <- 0

  # Call C code
  tmp <- .C("C_call_salmon", as.integer(iargs), as.numeric(dargs), 
                       as.numeric(t(V)), as.numeric(t(L)), as.numeric(t(W)), 
                       ret_H=as.numeric(ret_H), ret_ll=as.numeric(ret_ll), 
                       ret_conv=as.integer(ret_conv),
                       PACKAGE="SATS")

  # Set return objects
  ret_H  <- matrix(tmp$ret_H, nrow=k, ncol=n, byrow=TRUE)
  ret_H  <- setNames_H(ret_H, V, L, W)
  ret_ll <- tmp$ret_ll
  conv   <- tmp$ret_conv
  list(H=ret_H, loglike=ret_ll, converged=conv)

}

setNames_H <- function(H, V, L, W) {

  # Sets the row and column names for matrix H

  nms <- colnames(V)
  if (is.null(nms)) nms <- colnames(L)
  if (is.null(nms)) nms <- paste0("Sample", 1:ncol(V))
  colnames(H) <- nms
 
  nms <- colnames(W)
  if (is.null(nms)) nms <- paste0("deNovo", LETTERS[1:ncol(W)])
  rownames(H) <- nms
 
  H
}
