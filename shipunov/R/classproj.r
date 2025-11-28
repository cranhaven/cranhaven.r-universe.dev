## Visualize clusters by projecting them into a 2-dimensional space.
## 'data' must be numeric
Classproj <- function(data, classes, method="DMS") {
method <- match.arg(method, choices=c("DMS", "QJ"))
y <- as.matrix(data)[!is.na(classes), ]
cl <- classes[!is.na(classes)]
## from "clusterGeneration::viewClusters()"
## "DMS" projects data to the 2-dimensional space spanned by the first two eigenvectors of the between cluster distance matrix
## \eqn{B=\sum_{i=2}^{k_0}\sum_{j=1}^{i-1} n_i n_j(\theta_i-\theta_j)(\theta_i-\theta_j)^T}
## where thetai is the mean vector for i-th cluster.
## Dhillon I.S., Modha D.S., Spangler W.S. 2002. Class visualization of high-dimensional data with applications. Computational Statistics and Data Analysis. 41: 59--90.
## y -- nxp data points
## cl -- a partition of the n data points in y
## B Between cluster distance matrix measuring the between cluster variation
## Q Columns of Q are eigenvectors of the matrix B
.DMS <- function(y, cl) {
p <- ncol(y)
cl.set <- sort(unique(cl))
k0 <- length(cl.set)
n.set <- tapply(rep(1, length(cl)), cl, sum, na.rm=TRUE)
## obtain cluster centers and covariance matrices
mu.mat <- matrix(0, nrow=k0, ncol=p)
for (i in 1:k0) {
 yi <- y[cl == cl.set[i], ]
 if (n.set[i] > 1) {
  mu.mat[i, ] <- apply(yi, 2, mean, na.rm=TRUE)
 }
 else {
  mu.mat[i, ] <- yi
 }
}
if (k0 > 1) {
## B=\sum_{i=2}^{k_0}\sum_{j=1}^{i-1} n_in_j(\theta_i-\theta_j)(\theta_i-\theta_j)^T
 B <- matrix(0, nrow=p, ncol=p)
 for (i in 2:k0) {
  mui <- as.vector(mu.mat[i, ])
  for (j in 1:(i - 1)) {
   muj <- as.vector(mu.mat[j, ])
   ni <- n.set[i]
   nj <- n.set[j]
   tem1 <- (mui - muj)
   tem2 <- tem1 %*% t(tem1)
   B <- B + n.set[i] * n.set[j] * tem2
  }
 }
}
else {
    stop("Number of clusters is 0!\n")
}
eg <- eigen(B, symmetric=TRUE)
## The i-th column of Q is an eigenvector of B corresponding to the i-th eigenvalue
Q <- eg$vectors
res <- list(Q=Q, mu=mu.mat)
return(res)
}
## from "clusterGeneration::viewClusters()"
## "QJ" projects data to the 2-dimensional space spanned by the first two eigenvectors of the between cluster distance matrix
## \eqn{B={2\over k_0}\sum_{i=1}^{k_0}\Sigma_i+{2\over k_0(k_0-1)}\sum_{i<j}(\theta_i-\theta_j) (\theta_i-\theta_j)^T}
## where thetai and Sigmai Are the mean vector and covariance matrix for i-th cluster.
## Qiu W.-L., Joe H. 2006. Separation Index and Partial Membership for Clustering. Computational Statistics and Data Analysis. 50: 585--603.
## y -- nxp data points
## cl -- a partition of the n data points in y
## B Between cluster distance matrix measuring the between cluster variation
## Q Columns of Q are eigenvectors of the matrix B
.QJ <- function (y, cl) {
p <- ncol(y)
cl.set <- sort(unique(cl))
k0 <- length(cl.set)
## obtain cluster centers and covariance matrices
s <- array(0, c(p, p, k0))
mu.mat <- matrix(0, nrow=k0, ncol=p)
## W=sum_{i=1}^{k0} Sigma_i / k0
W <- matrix(0, nrow=p, ncol=p)
for (i in 1:k0) {
 yi <- y[cl == cl.set[i], ]
 mu.mat[i, ] <- apply(yi, 2, mean, na.rm=TRUE)
 s[, , i] <- cov(yi)
 W <- W + s[, , i]
}
W <- W/k0
if (k0 > 1) {
## B=sum_{i=1}^{k0-1}sum_{j=(i+1)}^{k0} (theta_i-theta_j)*(theta_i-theta_j)^T / (k0*(k0-1))
 B <- matrix(0, nrow=p, ncol=p)
 for (i in 1:(k0 - 1)) {
  mui <- mu.mat[i, ]
  for (j in (i + 1):k0) {
   muj <- mu.mat[j, ]
   B <- B + outer(mui - muj, mui - muj)
  }
 }
 B <- W + (B/(k0 * (k0 - 1)))
}
else {
 B <- W
}
B <- 2 * B
eg <- eigen(B, symmetric=TRUE)
Q <- eg$vectors
## The i-th column of Q is an eigenvector of B corresponding to the i-th eigenvalue
res <- list(Q=Q, mu=mu.mat)
return(res)
}
if (method=="QJ") res <- .QJ(y, cl)
if (method=="DMS") res <- .DMS(y, cl)
proj <- as.matrix(data) %*% res$Q[, 1:2]
centers <- res$mu %*% res$Q[, 1:2]
return(list(proj=proj, centers=centers))
}
