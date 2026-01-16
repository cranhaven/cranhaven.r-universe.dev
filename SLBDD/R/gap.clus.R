#' Gap statistics
#'
#' This function computes the gap and the number of groups using the gap statistics.
#'
#' @param DistanceMatrix Square matrix of GCC distances.
#' @param Clusters Matrix of member labels.
#' @param B Number of iterations for the bootstrap.
#'
#' @return A list containing:
#' \itemize{
#' \item - optim.k: number of groups.
#' \item - gap.values: gap values.
#' }
#'
#'
#' @import cluster
#'
#' @examples
#' data(TaiwanAirBox032017)
#' library(TSclust)
#' z <- diff(as.matrix(TaiwanAirBox032017[1:50,1:8]))
#' Macf <- as.matrix(diss(t(z), METHOD = "ACF", lag.max = 5))
#' sc1 <- hclust(as.dist(Macf), method = "complete")
#' memb <- cutree(sc1, 1:8)
#'
#' ngroups <- gap.clus(Macf, memb, 100)
#'
#'
#' @export
#'
#' @references Alonso, A. M. and Peña, D. (2019). Clustering time series by linear
#' dependency. \emph{Statistics and Computing}, 29(4):655–676.
#'

gap.clus <- function(DistanceMatrix, Clusters, B){

  N <- dim(Clusters)[1]
  nClus <- dim(Clusters)[2]

  W <-  WithinDispersion(DistanceMatrix, Clusters, nClus);

  mds <- cmdscale(DistanceMatrix,eig=TRUE)
  eps <- 2^(-52)
  f <-  sum(mds$eig > eps^(1/4))
  mds <- cmdscale(DistanceMatrix,eig=TRUE, k = f)

  Xmatrix <- mds$points

  svd <- svd(Xmatrix); U <- svd$u; V <- svd$v; D <- svd$d;
  Zmatrix = Xmatrix %*% V;
  Zmin = apply(Zmatrix, 2, min);
  Zmax = apply(Zmatrix, 2, max);

  Wstar = matrix(ncol = B, nrow = nClus)
  for (b in 1:B){

    for (ff in 1:f){
      Zmatrix[ ,ff] = runif(N, Zmin[ff], Zmax[ff]);
    }

    Zmatrix <- Zmatrix %*% t(V);
    ZDistanceMatrix = (dist(Zmatrix));
    L = hclust(dist(Zmatrix), method = "single");
    ZClusters = cutree(L, k = 1:nClus);
    ZDistanceMatrix <- as.matrix(ZDistanceMatrix)
    Wstar[,b] = WithinDispersion(ZDistanceMatrix, ZClusters, nClus);

  }

  logWmean = apply(log(Wstar), 1, mean);
  logWstd  = apply(log(Wstar), 1, sd)*sqrt(1 + 1/B);


  GAPstat = logWmean - log(W);

  WhoseK = GAPstat[1:nClus-1] - GAPstat[2:nClus] + logWstd[2:nClus];
  gap.values <- data.frame(gap = WhoseK)

  R <-  min(which(WhoseK >= 0))
  sal <- list(optim.k = R, gap.values = gap.values)
  return(sal)

}

WithinDispersion <- function(DistanceMatrix, Clusters, nClus){

  RW <- NULL
  for (k in 1:nClus){

    D <- NULL
    n <- NULL
    for (r in 1:k){
      Indexes <- which(Clusters[,k] == r)
      D[r] <- sum(sum(DistanceMatrix[Indexes,Indexes]))
      n[r] = 2*sum(length(Indexes));
    }
    RW[k] <- sum(D/n)
  }
  return(RW)
}
