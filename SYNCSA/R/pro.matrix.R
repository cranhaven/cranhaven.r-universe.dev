#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
pro.matrix<-function (mx1, mx2, x, my1 = NULL, my2 = NULL, y, permute.my2 = FALSE, permutations = 999,
                      norm = FALSE, norm.y = FALSE, strata = NULL, seqpermutation = NULL,
                      parallel = NULL, newClusters = TRUE, CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1] != permutations){
      stop("\n seqpermutation must be the dimension of permutations\n")
    }
  }
  x <- cbind(x)
  if(permute.my2){
    my1 <- as.matrix(my1)
    my2 <- as.matrix(my2)
  }
  y <- cbind(y)
  correlation <- procrustes.syncsa(x,y)
  N <- nrow(mx2)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, my1, my2, y, permute.my2, norm, norm.y){
    # x.permut <- mx1 %*% mx2[samp, , drop = FALSE]
    x.permut <- matmult.syncsa(mx1, mx2[samp, , drop = FALSE])
    if (norm) {
      matrix.permut <- apply(x.permut^2, 2, sum)
      x.permut <- sweep(x.permut, 2, sqrt(matrix.permut), "/")
    }
    if(permute.my2){
      # y.permut <- my1 %*% my2[samp, , drop = FALSE]
      y.permut <- matmult.syncsa(my1, my2[samp, , drop = FALSE])
      if (norm.y) {
        matrix.permut <- apply(y.permut^2, 2, sum)
        y.permut <- sweep(y.permut, 2, sqrt(matrix.permut), "/")
      }
    }
    if(!permute.my2){
      res <- SYNCSA::procrustes.syncsa(x.permut, y)
    }
    if(permute.my2){
      res <- SYNCSA::procrustes.syncsa(x.permut, y.permut)
    }
    return(res)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, y = y, permute.my2 = permute.my2, norm = norm, norm.y = norm.y)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, y = y, permute.my2 = permute.my2, norm = norm, norm.y = norm.y))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  sig <- (sum(value >= correlation) + 1)/(permutations + 1)
  return(list(Obs = correlation, p = sig))
}
