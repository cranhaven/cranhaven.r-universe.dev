#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
cor.procrustes <- function (x, y, permutations = 999, strata = NULL, na.rm = FALSE, seqpermutation = NULL,
                            parallel = NULL, newClusters = TRUE, CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1] != permutations){
      stop("\nThe seqpermutation must be the same dimension of permutations\n")
    }
  }
  correlation <- procrustes.syncsa(x, y)
  N <- nrow(x)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  ptest <- function(samp, x, y){
    res <- SYNCSA::procrustes.syncsa(x, y[samp, ,drop=FALSE])
    return(res)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], x = x, y = y)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, x = x,  y = y))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  signific <- (sum(abs(value) >= abs(correlation)) + 1)/(permutations + 1)
  res <- list(Obs = correlation, p = signific)
  return(res)
}
