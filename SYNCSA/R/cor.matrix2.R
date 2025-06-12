#' @rdname cor.matrix
#' @encoding UTF-8
#' @export
cor.matrix2<-function (mx1, mx2, x, y, method = "pearson", dist = "euclidean", put.together = NULL,
                      permutations = 999, strata = NULL, na.rm = FALSE,
                      seqpermutation = NULL, parallel = NULL, newClusters = TRUE, CL = NULL)
{
  if (!is.null(seqpermutation)) {
    if (dim(seqpermutation)[1] != permutations) {
      stop("\n The seqpermutation must be the same dimension of permutations\n")
    }
  }
  mx1 <- as.matrix(mx1)
  mx2 <- as.matrix(mx2)
  x <- as.matrix(x)
  y <- as.matrix(y)
  dist.y <- vegan::vegdist(y, method = dist, na.rm = na.rm)
  dist.x <- vegan::vegdist(x, method = dist, na.rm = na.rm)
  correlation <- stats::cor(dist.x, dist.y, method = method)
  N <- nrow(mx2)
  if (is.null(seqpermutation)) {
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if (!is.null(CL)) {
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, dist.y, dist, na.rm, method, put.together) {
    x.permut <- cbind(rao.diversity(mx1, traits = mx2[samp, , drop = FALSE], checkdata = FALSE, put.together = put.together)$FunRao)
    dist.x.permut <- vegan::vegdist(x.permut, method = dist, na.rm = na.rm)
    cor.x.permut <- stats::cor(dist.x.permut, dist.y, method = method)
    return(cor.x.permut)
  }
  if (is.null(parallel)) {
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1:permutations) {
      value[i, ] <- ptest(samp = seqpermutation[i, ], mx1 = mx1,
                          mx2 = mx2, dist.y = dist.y, dist = dist,
                          na.rm = na.rm, method = method, put.together = put.together)
    }
  }
  else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation,
                                       ptest, mx1 = mx1, mx2 = mx2, dist.y = dist.y,
                                       dist = dist, na.rm = na.rm, method = method, put.together = put.together))
    if (newClusters) {
      parallel::stopCluster(CL)
    }
  }
  signific <- (sum(abs(value) >= abs(correlation)) + 1)/(permutations + 1)
  res <- list(Obs = correlation, p = signific)
  return(res)
}
