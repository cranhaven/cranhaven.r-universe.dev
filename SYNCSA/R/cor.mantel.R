#' @rdname cor.matrix
#' @include cor.matrix.R
#' @encoding UTF-8
#' @export
cor.mantel <- function (dist.x, dist.y, method = "pearson", permutations = 999, strata = NULL,
                        na.rm = FALSE, seqpermutation = NULL, parallel = NULL, newClusters = TRUE,
                        CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1] != permutations){
      stop("\n seqpermutation must be the same dimension of permutations.\n")
    }
  }
  correlation <- cor(dist.x, dist.y, method = method)
  mx <- as.matrix(dist.x)
  N <- nrow(mx)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  if (na.rm){
    use <- "complete.obs"
  } else {
    use <- "all.obs"
  }
  ptest <- function(samp,mx,dist.y,method,use) {
    res <- cor(stats::as.dist(mx[samp, samp]), dist.y, method = method, use = use)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], mx = mx, dist.y = dist.y, method = method, use = use)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx = mx, dist.y = dist.y, method = method, use = use))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  signific <- (sum(abs(value) >= abs(correlation)) + 1)/(permutations + 1)
  res <- list(Obs = correlation, p = signific)
  return(res)
}
