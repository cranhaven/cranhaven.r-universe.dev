Clustergram <- function(data, maxcl=ncol(data)*2, nboot=FALSE,
 method="kmeans", m.dist="euclidean", m.hclust="complete",
 plot=TRUE, broom=4e-3, col="gray", ...)
{
 method <- match.arg(method, choices=c("kmeans", "hclust"))
 .makem <- function(data, maxcl, method, m.dist, m.hclust, broom)
 {
  Z <- Y <- matrix(0, ncol=maxcl, nrow=nrow(data))
  broom <- rep(broom, nrow(data))
  if (method == "hclust") hcl <- hclust(dist(data, method=m.dist), method=m.hclust)
  for(k in 2:maxcl)
  {
   if (method == "kmeans")
   {
    km <- kmeans(data, centers=k, nstart=k)
    clusters <- km$cluster
    centers <- km$centers
   }
   if (method == "hclust")
   {
    clusters <- cutree(hcl, k=k)
    centers <- as.matrix(aggregate(data, list(clusters), mean)[, -1])
   }
   wmeans <- centers %*% prcomp(data)$rotation[, 1]
   brooms <- unlist(tapply(broom, clusters, cumsum))[order(seq_along(clusters)[order(clusters)])]
   Y[, k] <- wmeans[clusters] + brooms
   Z[, k] <- clusters
  }
  Z[, 1] <- 1
  list(Y=Y, Z=Z)
 }
 ##
 data <- scale(data) ## data must be scaled
 M <- .makem(data=data, maxcl=maxcl, method=method, m.dist=m.dist, m.hclust=m.hclust, broom=broom)
 if (nboot)
 {
  M$Y <- M$Y[, rep(1:maxcl, nboot+1)]
  M$Z <- M$Z[, rep(1:maxcl, nboot+1)]
  for (n in 1:nboot)
  {
   ndata <- data[sample(1:nrow(data), replace=TRUE), ]
   ndata <- scale(ndata)
   MM <- .makem(data=data, maxcl=maxcl, method=method, m.dist=m.dist, m.hclust=m.hclust, broom=broom)
   M$Y[, ((maxcl*nboot)+1):(maxcl*(nboot+1))] <- MM$Y
   M$Z[, ((maxcl*nboot)+1):(maxcl*(nboot+1))] <- MM$Z
  }
  X <- matrix(rep(rep(1:maxcl, each=nrow(data)), nboot+1), ncol=maxcl*(nboot+1), nrow=nrow(data))
 } else {
  X <- matrix(rep(1:maxcl, each=nrow(data)), ncol=maxcl, nrow=nrow(data))
 }
 ##
 if (plot) {
  PC1 <- data %*% prcomp(data)$rotation[, 1]
  plot(1:maxcl, ylim=range(PC1), type="n", axes=FALSE, xlab="", ylab="", ...)
  axis(side=1, at=1:maxcl)
  abline(v=1:maxcl, lty=3)
  title(xlab="Number of clusters")
  title(ylab="PCA-weighted cluster means", line=1)
  matlines(t(X), t(M$Y), col=col, lty=1)
 }
 ##
 invisible(M$Z)
}
