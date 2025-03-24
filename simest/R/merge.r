fastmerge <- function(DataMat, w = NULL, tol = 1e-04){
  DataMat <- as.matrix(DataMat)
  p <- ncol(DataMat)
  x <- DataMat[,1]
  n <- length(x)
  if(is.null(w)){ w <- rep(1,n) }
  xx <- tol*floor({x - floor(x)}/tol) + floor(x)
  nd <- !duplicated(xx)
  ux <- sort(x[nd])
  uxx <- sort(xx[nd])
  nx <- length(ux)
  if (nx == n) {
    ox <- TRUE
    tmp <- cbind(w, DataMat, 0)
  } else {
    ox <- match(xx, uxx)
    tapply1 <- function(X, INDEX, FUN = NULL, ..., simplify = TRUE){
      sapply(X = unname(split(X, INDEX)), FUN = FUN, ..., simplify = simplify, USE.NAMES = FALSE)
    }
    foo <- function(i, D, q)
      if(length(i)==1){
        return(c(sum(q[i]), colMeans(D[i,,drop = FALSE]),0 ))
      } else {
        return(c(sum(q[i]), colMeans(D[i,,drop = FALSE]),var(D[i,2])*(length(i)-1)))
      }
    tmp <- matrix(
      unlist(
        tapply1(seq_len(n), ox, foo, D = DataMat, q = w),
        use.names = FALSE),
      ncol = p+2, byrow = TRUE
    )
  }
  w <- tmp[, 1L]
  DataMat <- tmp[, -1L]
  DataMat[,1] <- tol*floor({DataMat[,1] - floor(DataMat[,1])}/tol) + floor(DataMat[,1])  
  return(list(DataMat = DataMat[,-ncol(DataMat)], w = w, AddVar = DataMat[,ncol(DataMat)]))
}