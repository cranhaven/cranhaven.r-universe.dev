#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
cor.matrix.partial <- function (mx1, mx2, x, my1 = NULL, my2 = NULL, y, mz1 = NULL, mz2 = NULL, z,
                                method = "pearson", dist = "euclidean", permute.my2 = FALSE,
                                permute.mz2 = FALSE, permutations = 999, norm = FALSE, norm.y = FALSE,
                                norm.z = FALSE, strata = NULL, na.rm = FALSE, seqpermutation = NULL,
                                parallel = NULL, newClusters = TRUE, CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1]!=permutations){
      stop("\n seqpermutation must be the same dimension of permutations.\n")
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
  dist.x <- vegan::vegdist(x, method = dist, na.rm = na.rm)
  dist.y <- vegan::vegdist(y, method = dist, na.rm = na.rm)
  dist.z <- vegan::vegdist(z, method = dist, na.rm = na.rm)
  rxy <- stats::cor(dist.x, dist.y, method = method)
  rxz <- stats::cor(dist.x, dist.z, method = method)
  ryz <- stats::cor(dist.y, dist.z, method = method)
  statistic <- part.cor(rxy, rxz, ryz)
  N <- nrow(mx2)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, my1, my2, dist.y, mz1, mz2, dist.z, permute.my2, permute.mz2, norm, norm.y, norm.z, dist, na.rm, method){
    # x.permut <- mx1 %*% mx2[samp, , drop = FALSE]
    x.permut <- matmult.syncsa(mx1, mx2[samp, , drop = FALSE])
    if (norm) {
      matrix.permut <- apply(x.permut^2, 2, sum)
      x.permut <- sweep(x.permut, 2, sqrt(matrix.permut), "/")
    }
    dist.x.permut <- vegan::vegdist(x.permut, method = dist, na.rm = na.rm)
    if(permute.my2){
      # y.permut <- my1 %*% my2[samp, , drop = FALSE]
      y.permut <- matmult.syncsa(my1, my2[samp, , drop = FALSE])
      if (norm.y) {
        matrix.permut <- apply(y.permut^2, 2, sum)
        y.permut <- sweep(y.permut, 2, sqrt(matrix.permut), "/")
      }
      dist.y.permut <- vegan::vegdist(y.permut, method = dist, na.rm = na.rm)
    }
    if(permute.mz2){
      # z.permut <- mz1 %*% mz2[samp, , drop = FALSE]
      z.permut <- matmult.syncsa(mz1, mz2[samp, , drop = FALSE])
      if (norm.z) {
        matrix.permut <- apply(z.permut^2, 2, sum)
        z.permut <- sweep(z.permut, 2, sqrt(matrix.permut), "/")
      }
      dist.z.permut <- vegan::vegdist(z.permut, method = dist, na.rm = na.rm)
    }
    if(!permute.my2 & !permute.mz2){
      rxy.temp <- stats::cor(dist.x.permut, dist.y, method = method)
      rxz.temp <- stats::cor(dist.x.permut, dist.z, method = method)
      ryz.temp <- ryz
    }
    if(permute.my2 & !permute.mz2){
      rxy.temp <- stats::cor(dist.x.permut, dist.y.permut, method = method)
      rxz.temp <- stats::cor(dist.x.permut, dist.z, method = method)
      ryz.temp <- stats::cor(dist.y.permut, dist.z, method = method)
    }
    if(!permute.my2 & permute.mz2){
      rxy.temp <- stats::cor(dist.x.permut, dist.y, method = method)
      rxz.temp <- stats::cor(dist.x.permut, dist.z.permut, method = method)
      ryz.temp <- stats::cor(dist.y, dist.z.permut, method = method)
    }
    if(permute.my2 & permute.mz2){
      rxy.temp <- stats::cor(dist.x.permut, dist.y.permut, method = method)
      rxz.temp <- stats::cor(dist.x.permut, dist.z.permut, method = method)
      ryz.temp <- stats::cor(dist.y.permut, dist.z.permut, method = method)
    }
    res<- SYNCSA::part.cor(rxy.temp, rxz.temp, ryz.temp)
    return(res)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, dist.y = dist.y, mz1 = mz1, mz2 = mz2, dist.z = dist.z, permute.my2 = permute.my2, permute.mz2 = permute.mz2, norm = norm, norm.y = norm.y, norm.z = norm.z, dist = dist, na.rm = na.rm, method = method)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, dist.y = dist.y, mz1 = mz1, mz2 = mz2, dist.z = dist.z, permute.my2 = permute.my2, permute.mz2 = permute.mz2, norm = norm, norm.y = norm.y, norm.z = norm.z, dist = dist, na.rm = na.rm, method = method))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  signif <- (sum(abs(value) >= abs(statistic)) + 1)/(permutations + 1)
  res <- list(Obs = statistic, p = signif)
  return(res)
}
