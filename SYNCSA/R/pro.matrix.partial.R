#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
pro.matrix.partial <- function (mx1, mx2, x, my1 = NULL, my2 = NULL, y, mz1 = NULL, mz2 = NULL, z,
                                permute.my2 = FALSE, permute.mz2 = FALSE, permutations = 999,
                                norm = FALSE, norm.y = FALSE, norm.z = FALSE, strata = NULL,
                                seqpermutation = NULL, parallel = NULL, newClusters = TRUE, CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1] != permutations){
      stop("\n The seqpermutation must be the dimension of permutations\n")
    }
  }
  mx1 <- as.matrix(mx1)
  mx2 <- as.matrix(mx2)
  x <- as.matrix(x)
  if(permute.my2){
    my1 <- as.matrix(my1)
    my2 <- as.matrix(my2)
  }
  y <- as.matrix(y)
  if(permute.mz2){
    mz1 <- as.matrix(mz1)
    mz2 <- as.matrix(mz2)
  }
  z <- as.matrix(z)
  statistic <- procrustes.partial(x, y, z)
  N <- nrow(mx2)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, my1, my2, mz1, mz2, permute.my2, permute.mz2, norm, norm.y, norm.z){
    # x.permut <- mx1 %*% mx2[samp, ,drop = FALSE]
    x.permut <- matmult.syncsa(mx1, mx2[samp, ,drop = FALSE])
    if (norm) {
      matrix.permut <- apply(x.permut^2, 2, sum)
      x.permut <- sweep(x.permut, 2, sqrt(matrix.permut), "/")
    }
    if(permute.my2){
      # y.permut <- my1 %*% my2[samp, ,drop = FALSE]
      y.permut <- matmult.syncsa(my1, my2[samp, ,drop = FALSE])
      if (norm.y) {
        matrix.permut <- apply(y.permut^2, 2, sum)
        y.permut <- sweep(y.permut, 2, sqrt(matrix.permut), "/")
      }
    }
    if(permute.mz2){
      # z.permut <- mz1 %*% mz2[samp, ,drop = FALSE]
      z.permut <- matmult.syncsa(mz1, mz2[samp, ,drop = FALSE])
      if (norm.z) {
        matrix.permut <- apply(z.permut^2, 2, sum)
        z.permut <- sweep(z.permut, 2, sqrt(matrix.permut), "/")
      }
    }
    if(!permute.my2 & !permute.mz2){
      res <- SYNCSA::procrustes.partial(x.permut, y, z)
    }
    if(permute.my2 & !permute.mz2){
      res <- SYNCSA::procrustes.partial(x.permut, y.permut, z)
    }
    if(!permute.my2 & permute.mz2){
      res <- SYNCSA::procrustes.partial(x.permut, y, z.permut)
    }
    if(permute.my2 & permute.mz2){
      res <- SYNCSA::procrustes.partial(x.permut, y.permut, z.permut)
    }
    return(res)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, mz1 = mz1, mz2 = mz2, permute.my2 = permute.my2, permute.mz2 = permute.mz2, norm = norm, norm.y = norm.y, norm.z = norm.z)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, mz1 = mz1, mz2 = mz2, permute.my2 = permute.my2, permute.mz2 = permute.mz2, norm = norm, norm.y = norm.y, norm.z = norm.z))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  signif <- (sum(abs(value) >= abs(statistic)) + 1)/(permutations + 1)
  res <- list(Obs = statistic, p = signif)
  return(res)
}
