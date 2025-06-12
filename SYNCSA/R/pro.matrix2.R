#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
pro.matrix2<-function (mx1, mx2, x, y, put.together = NULL, permutations = 999, strata = NULL,
          seqpermutation = NULL, parallel = NULL, newClusters = TRUE,
          CL = NULL)
{
  if (!is.null(seqpermutation)) {
    if (dim(seqpermutation)[1] != permutations) {
      stop("\n seqpermutation must be the dimension of permutations\n")
    }
  }
  x <- cbind(x)
  y <- cbind(y)
  correlation <- procrustes.syncsa(x, y)
  N <- nrow(mx2)
  if (is.null(seqpermutation)) {
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if (!is.null(CL)) {
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, y, put.together) {
    x.permut <- cbind(rao.diversity(mx1, traits = mx2[samp, , drop = FALSE], checkdata = FALSE, put.together = put.together)$FunRao)
    res <- SYNCSA::procrustes.syncsa(x.permut, y)
    return(res)
  }
  if (is.null(parallel)) {
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1:permutations) {
      value[i, ] <- ptest(samp = seqpermutation[i, ], mx1 = mx1, mx2 = mx2, y = y, put.together = put.together)
    }
  }
  else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx1 = mx1, mx2 = mx2, y = y, put.together = put.together))
    if (newClusters) {
      parallel::stopCluster(CL)
    }
  }
  sig <- (sum(value >= correlation) + 1)/(permutations + 1)
  return(list(Obs = correlation, p = sig))
}
