#' HSIC cluster permutation conditional independence test
#'
#' Conditional independence test using HSIC and permutation with clusters.
#'
#' @details Let x and y be two samples of length n. Gram matrices K and L are defined as: \eqn{K_{i,j} = \exp\frac{(x_i-x_j)^2}{\sigma^2}}{K_{i,j} =exp((x_i-x_j)^2/sig^2)}, \eqn{L_{i,j} = \exp\frac{(y_i-y_j)^2}{\sigma^2}}{L_{i,j} =exp((y_i-y_j)^2/sig^2)} and \eqn{M_{i,j} = \exp\frac{(z_i-z_j)^2}{\sigma^2}}{M_{i,j} =exp((z_i-z_j)^2/sig^2)}. \eqn{H_{i,j} = \delta_{i,j} - \frac{1}{n}}{H_{i,j} = delta_{i,j} - 1/n}. Let \eqn{A=HKH}, \eqn{B=HLH} and \eqn{C=HMH}. \eqn{HSIC(X,Y|Z) = \frac{1}{n^2}Tr(AB-2AC(C+\epsilon I)^{-2}CB+AC(C+\epsilon I)^{-2}CBC(C+\epsilon I)^{-2}C)}{HSIC(X,Y|Z) = Tr(AB-2AC(C+\epsilon I)^{-2}CB+AC(C+\epsilon I)^{-2}CBC(C+\epsilon I)^{-2}C)/n^2}. Permutation test clusters Z and then permutes Y in the clusters of Z p times to get \eqn{Y_{(p)}} and calculates \eqn{HSIC(X,Y_{(p)}|Z)}. \eqn{pval = \frac{1(HSIC(X,Y|Z)>HSIC(Z,Y_{(p)}|Z))}{p}}{(HSIC(X,Y|Z)>HSIC(X,Y_{(p)}|Z))/p}.
#' @param x first variable
#' @param y second variable
#' @param z set of variables on which we condition
#' @param sig the with of the Gaussian kernel
#' @param p the number of permutations
#' @param numCluster number of clusters for clustering z
#' @param numCol maximum number of columns that we use for the incomplete Cholesky decomposition
#' @param eps normalization parameter for HSIC cluster test
#' @param paral number of cores used
#'
#' @importFrom kernlab inchol rbfdot
#' @importFrom parallel makeCluster clusterEvalQ parLapply stopCluster
#' @importFrom stats var kmeans pgamma
#' @import methods
#'
#' @references Tillman, R. E., Gretton, A. and Spirtes, P. (2009). Nonlinear directed acyclic structure learning with weakly additive noise model. NIPS 22, Vancouver.
#' @references K. Fukumizu et al. (2007). Kernel Measures of Conditional Dependence. NIPS 20. \url{https://papers.nips.cc/paper/3340-kernel-measures-of-conditional-dependence.pdf}
#
#' @export
#' @return hsic.clust() returns a list with class htest containing
#' \item{method}{description of test}
#' \item{statistic}{observed value of the test statistic}
#' \item{estimate}{HSIC(x,y)}
#' \item{estimates}{a vector: [HSIC(x,y), mean of HSIC(x,y), variance of HSIC(x,y)]}
#' \item{replicates}{replicates of the test statistic}
#' \item{p.value}{approximate p-value of the test}
#' \item{data.name}{desciption of data}
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk}) and Nina Ines Bertille Desgranges
#' @seealso \link{hsic.gamma}, \link{hsic.perm}, \link{kernelCItest}
#' @examples
#' library(energy)
#' set.seed(10)
#' # x and y dependent, but independent conditionally on z
#' z <- 10*runif(300)
#' x <- sin(z) + runif(300)
#' y <- cos(z) + runif(300)
#' plot(x,y)
#' hsic.gamma(x,y)
#' hsic.perm(x,y)
#' dcov.test(x,y)
#' hsic.clust(x,y,z)


hsic.clust <- function(                x,             # the first variable for cond ind
                                       y,             # the second variable for cond ind
                                       z,             # the set of variables to wich I condition
                                       sig=1,         # the width of gaussian kernel
                                       p=100,         # number of permutation of 1-alpha level test
                                       numCluster=10, #number of clusters if we use kpc cluster permutation
                                       numCol=50,      # number of column in Incomplete Cholesky Decomposition
                                       eps=0.1,       #normalization parameter
                                       paral=1       # number of cores
                                       ){
  x = as.matrix(x)
  y = as.matrix(y)
  z = as.matrix(z)
  n = nrow(as.matrix(x))
  H = diag(n)-1/n*matrix(rep(x = 1, length.out = n*n), n, n)
  pval.perm=matrix(0,p)
  ###
  tempx = inchol(as.matrix(x), kernel = 'rbfdot', kpar = list(sigma = 1/sig), maxiter = numCol)
  iCHx = tempx@.Data
  tempy = inchol(as.matrix(y), kernel = 'rbfdot', kpar = list(sigma = 1/sig), maxiter = numCol)
  iCHy = tempy@.Data
  tempz = inchol(as.matrix(z), kernel = 'rbfdot', kpar = list(sigma = 1/sig), maxiter = numCol)
  iCHz = tempz@.Data
  a = svd(x = H%*%iCHx)
  Ux = a$u
  Sx = a$d
  ##
  b = svd(x = H%*%iCHy)
  Uy = b$u
  Sy = b$d
  ##
  c = svd(x = H%*%iCHz)
  Uz = c$u
  Sz = c$d
  ##
  if (length(Sx) != 1){ MSx <- diag(Sx^2) }  else {MSx <- as.matrix(Sx^2)}
  if (length(Sy) != 1){ MSy <- diag(Sy^2) }  else {MSy <- as.matrix(Sy^2)}
  if (length(Sz) != 1){ MSz <- diag(Sz^2/(Sz^2+eps)) }  else {MSz <- as.matrix(Sz^2/(Sz^2+eps))}
  ##
  hsic = 1/(n*n)*sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Uy%*%MSy))%*%t(Uy)  -2*Ux%*%((((MSx%*%t(Ux))%*%(Uz%*%MSz))%*%t(Uz))%*%(Uy%*%MSy))%*%t(Uy) + Ux%*%((((MSx%*%t(Ux))%*%(Uz%*%MSz))%*%t(Uz))%*%(Uy%*%((MSy%*%t(Uy))%*%(Uz%*%MSz))))%*%t(Uz)))
  if (paral == 1){
    pval.perm = lapply(X = 1:p, function(m, Ux, Sx, Uy, Sy, Uz, Sz, sig, numCol, numCluster) {
      t = kmeans(z,numCluster)
      perm = c(1:n)
      for (j in 1:numCluster){
        perm[t$cluster==j] = perm[t$cluster==j][sample(sum(t$cluster==j))]
      }
      ##
      Uyp <- Uy[perm,]
      result = 1/(n*n)*sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Uyp%*%MSy))%*%t(Uyp)  -2*Ux%*%((((MSx%*%t(Ux))%*%(Uz%*%MSz))%*%t(Uz))%*%(Uyp%*%MSy))%*%t(Uyp) + Ux%*%((((MSx%*%t(Ux))%*%(Uz%*%MSz))%*%t(Uz))%*%(Uyp%*%((MSy%*%t(Uyp))%*%(Uz%*%MSz))))%*%t(Uz)))
      return (  result ) },  Ux=Ux, Sx=Sx, Uy=Uy, Sy=Sy, Uz=Uz, Sz=Sz, sig, numCol, numCluster=numCluster)
  }
  else {
    cl <- makeCluster(mc <- getOption('cl.cores',paral))
    clusterEvalQ(cl, library(kernlab))
    clusterEvalQ(cl, library(psych))
    #clusterEvalQ(cl, source('kpc_functions.R'))?clust
    pval.perm <- parLapply(cl = cl, X = 1:p, function(m, Ux, Sx, Uy, Sy, Uz, Sz, sig, eps, numCol, numCluster) { m
      t <- kmeans(z,numCluster)
      perm <- c(1:n)
      for (j in 1:numCluster){
        perm[t$cluster==j] <- perm[t$cluster==j][sample(sum(t$cluster==j))]
      }
      ##
      Uyp <- Uy[perm,]
      result <- 1/(n*n)*sum(diag( Ux%*%((diag(Sx^2)%*%t(Ux))%*%(Uyp%*%diag(Sy^2)))%*%t(Uyp)  -2*Ux%*%((((diag(Sx^2)%*%t(Ux))%*%(Uz%*%diag(Sz^2/(Sz^2+eps))))%*%t(Uz))%*%(Uyp%*%diag(Sy^2)))%*%t(Uyp) + Ux%*%((((diag(Sx^2)%*%t(Ux))%*%(Uz%*%diag(Sz^2/(Sz^2+eps))))%*%t(Uz))%*%(Uyp%*%((diag(Sy^2)%*%t(Uyp))%*%(Uz%*%diag(Sz^2/(Sz^2+eps))))))%*%t(Uz)))
      return (  result ) },  Ux=Ux, Sx=Sx, Uy=Uy, Sy=Sy, Uz=Uz, Sz=Sz, sig=sig, eps=eps, numCol=numCol, numCluster=numCluster)
    stopCluster(cl)
  }
  pval.perm2 <- matrix(0,ncol=p)
  for (i in 1:p){pval.perm2[i]<-pval.perm[[i]]}
  pval <- mean(c(pval.perm2,hsic)>=hsic)

  HSMean <- mean(pval.perm2)
  HSVariance <- var(pval.perm2)

  names(hsic) <- "HSIC"
  names(HSMean) <-"HSIC mean"
  names(HSVariance) <- "HSIC variance"
  dataname <- paste("Cluster permutation approximation", sep = "")
  e <- list(method = paste("HSIC test of conditional independence", sep = ""),
            statistic = hsic,
            estimate = hsic,
            estimates = c(hsic,HSMean,HSVariance),
            p.value = pval,
            replicates = pval.perm2,
            data.name = dataname)
  class(e) <- "htest"
  return(e)

}
