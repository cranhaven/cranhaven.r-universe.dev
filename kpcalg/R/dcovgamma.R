#'
#' Test to check the independence between two variables x and y using the Distance Covariance.
#' The dcov.gamma() function, uses Distance Covariance independence criterion with gamma approximation to test for independence between two random variables.
#'
#' @details Let x and y be two samples of length n. Gram matrices K and L are defined as: \eqn{K_{i,j} = \| x_i-x_j \|^s}{K_{i,j} =|x_i-x_j|^s} and \eqn{L_{i,j} = \| y_i-y_j \|^s}{L_{i,j} =|y_i-y_j|^s}, where 0<s<2. \eqn{H_{i,j} = \delta_{i,j} - \frac{1}{n}}{H_{i,j} = delta_{i,j} - 1/n}. Let A=HKH and B=HLH, then \eqn{nV^2=\frac{1}{n^2}\sum A_{i,j} B_{i,j}}{nV^2 = \sum A_{i,j} B_{i,j} \n^2}. For more detail: \link{dcov.test} in package energy. Gamma test compares \eqn{nV^2_n(x,y)} with the \eqn{\alpha}{alpha} quantile of the gamma distribution with mean and variance same as \eqn{nV^2_n} under independence hypothesis.
#' @param x data of first sample
#' @param y data of second sample
#' @param index exponent on Euclidean distance, in (0,2]
#' @param numCol Number of columns used in incomplete Singular Value Decomposition
#'
#' @references A. Gretton et al. (2005). Kernel Methods for Measuring Independence. JMLR 6 (2005) 2075-2129.
#' @references G. Szekely, M. Rizzo and N. Bakirov (2007). Measuring and Testing Dependence by Correlation of Distances. The Annals of Statistics 2007, Vol. 35, No. 6, 2769-2794.
#'
#' @importFrom RSpectra eigs
#' @importFrom stats var pgamma dist
#' @import methods
#'
#' @export
#' @return dcov.gamma() returns a list with class htest containing
#' \item{method}{description of test}
#' \item{statistic}{observed value of the test statistic}
#' \item{estimate}{nV^2(x,y)}
#' \item{estimates}{a vector: [nV^2(x,y), mean of nV^2(x,y), variance of nV^2(x,y)]}
#' \item{replicates}{replicates of the test statistic}
#' \item{p.value}{approximate p-value of the test}
#' \item{data.name}{desciption of data}
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk}) and Nina Ines Bertille Desgranges
#' @seealso \link{hsic.perm}, \link{hsic.clust}, \link{hsic.gamma}, \link{dcov.test}, \link{kernelCItest}
#' @examples
#' library(energy)
#' set.seed(10)
#' #independence
#' x <- runif(300)
#' y <- runif(300)
#'
#' hsic.gamma(x,y)
#' hsic.perm(x,y)
#' dcov.gamma(x,y)
#' dcov.test(x,y)
#'
#' #uncorelated but not dependent
#' z <- 10*(runif(300)-0.5)
#' w <- z^2 + 10*runif(300)
#'
#' cor(z,w)
#' hsic.gamma(z,w)
#' hsic.perm(z,w)
#' dcov.gamma(z,w)
#' dcov.test(z,w)

dcov.gamma <- function(x, # first variable
                       y, # second variable
                       index=1, #type of distance
                       numCol=100 #number of columns used in incomplete SVD decomposition
){
  n <- length(x)
  m <- length(y)
  if (index < 0 || index > 2) {
    warning("index must be in [0,2), using default index=1")
    index = 1
  }
  if (n != m)
    stop("Sample sizes must agree")
  if (!(all(is.finite(c(x, y)))))
    stop("Data contains missing or infinite values")
  H <- diag(n)-matrix(1/n,n,n)
  matx <- as.matrix(dist(x))
  maty <- as.matrix(dist(y))
  ##
  P <- eigs(matx,numCol)
  Q <- eigs(maty,numCol)
  Ux <- P$vectors
  Sx <- diag(P$values)
  Uy <- Q$vectors
  Sy <- diag(Q$values)
  ##
  nV2 <- sum(diag( (H%*%Ux) %*%Sx %*% ((t(Ux)%*%H) %*% (H%*%Uy)) %*%Sy%*% (t(Uy)%*%H) ))/n
  nV2Mean <- mean(matx)*mean(maty)
  nV2Variance <- 2*(n-4)*(n-5)/n/(n-1)/(n-2)/(n-3) * sum(diag( (H%*%Ux) %*%Sx %*% ((t(Ux)%*%H) %*% (H%*%Ux)) %*%Sx%*% (t(Ux)%*%H) )) * sum(diag( (H%*%Uy) %*%Sy %*% ((t(Uy)%*%H) %*% (H%*%Uy)) %*%Sy%*% (t(Uy)%*%H) ))/n^4 * n^2
  alpha <- (nV2Mean)^2/nV2Variance
  beta  <- nV2Variance/(nV2Mean)
  pval <- 1-pgamma(q = nV2, shape = alpha, rate = 1/beta)
  dCov <- sqrt(nV2/n)

  names(dCov) <- "dCov"
  names(nV2) <- "nV^2"
  names(nV2Mean) <-"nV^2 mean"
  names(nV2Variance) <- "nV^2 variance"
  dataname <- paste("index 1, Gamma approximation", sep = "")
  e <- list(method = paste("dCov test of independence", sep = ""),
            statistic = nV2,
            estimate = dCov,
            estimates = c(nV2,nV2Mean,nV2Variance),
            p.value = pval,
            replicates = NULL,
            data.name = dataname)
  class(e) <- "htest"
  return(e)

}


