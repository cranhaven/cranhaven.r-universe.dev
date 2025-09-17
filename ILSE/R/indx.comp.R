indx.comp <- function(Xmat){
  p <- ncol(Xmat)
  IDX <- NULL
  for (j in 1:p){
    idxj <- list(which(!is.na(Xmat[,j])))
    IDX <- c(IDX,idxj)
  }
  IDX
}
