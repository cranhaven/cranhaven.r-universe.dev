#' Hilber Schmidt Independence Criterion gamma test
#'
#' Test to check the independence between two variables x and y using HSIC.
#' The hsic.gamma() function, uses Hilbert-Schmidt independence criterion to test for independence between
#' random variables.
#'
#' @details Let x and y be two samples of length n. Gram matrices K and L are defined as: \eqn{K_{i,j} = \exp\frac{(x_i-x_j)^2}{\sigma^2}}{K_{i,j} =exp((x_i-x_j)^2/sig^2)} and \eqn{L_{i,j} = \exp\frac{(y_i-y_j)^2}{\sigma^2}}{L_{i,j} =exp((y_i-y_j)^2/sig^2)}. \eqn{H_{i,j} = \delta_{i,j} - \frac{1}{n}}{H_{i,j} = delta_{i,j} - 1/n}. Let \eqn{A=HKH} and \eqn{B=HLH}, then \eqn{HSIC(x,y)=\frac{1}{n^2}Tr(AB)}{HSIC(x,y)=Tr(AB)/n^2}. Gamma test compares HSIC(x,y) with the \eqn{\alpha}{alpha} quantile of the gamma distribution with mean and variance such as HSIC under independence hypothesis.
#' @param x data of first sample
#' @param y data of second sample
#' @param sig Gaussian kernel width for HSIC tests. Default is 1
#' @param numCol maximum number of columns that we use for the incomplete Cholesky decomposition
#'
#' @importFrom kernlab inchol rbfdot
#' @importFrom stats var kmeans pgamma
#' @import methods
#'
#' @references A. Gretton et al. (2005). Kernel Methods for Measuring Independence. JMLR 6 (2005) 2075-2129.
#'
#' @export
#' @return hsic.gamma() returns a list with class htest containing
#' \item{method}{description of test}
#' \item{statistic}{observed value of the test statistic}
#' \item{estimate}{HSIC(x,y)}
#' \item{estimates}{a vector: [HSIC(x,y), mean of HSIC(x,y), variance of HSIC(x,y)]}
#' \item{replicates}{replicates of the test statistic}
#' \item{p.value}{approximate p-value of the test}
#' \item{data.name}{desciption of data}
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk}) and Nina Ines Bertille Desgranges
#' @seealso \link{hsic.perm}, \link{hsic.clust}, \link{kernelCItest}
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

hsic.gamma <- function(x, # first variable
                       y, # second variable
                       sig=1, #sigma for the Gaussian kernel
                       numCol=100
                       ){
  n <- length(x)
  H <- diag(n)-matrix(1/n,n,n)
  rbf <- rbfdot(sigma=1/sig)
  tempx <- inchol(as.matrix(x), kernel = rbf, maxiter = numCol)
  iCHx <- tempx@.Data
  a <- svd(x = H%*%iCHx)
  Ux <- a$u
  Sx <- a$d
  ##
  tempy <- inchol(as.matrix(y), kernel = rbf, maxiter = numCol)
  iCHy <- tempy@.Data
  b <- svd(x = H%*%iCHy)
  Uy <- b$u
  Sy <- b$d
  ##
  if (length(Sx) != 1){ MSx <- diag(Sx^2) }  else {MSx <- as.matrix(Sx^2)}
  if (length(Sy) != 1){ MSy <- diag(Sy^2) }  else {MSy <- as.matrix(Sy^2)}
  hsic <- sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Uy%*%MSy))%*%t(Uy) )) /n^2
  K <- crossprod(t(iCHx))
  L <- crossprod(t(iCHy))
  mux <- 1/(n*(n-1))*sum(K-diag(K)*diag(ncol(K)))
  muy <- 1/(n*(n-1))*sum(L-diag(L)*diag(ncol(L)))
  HSMean <- 1/n * (1 + mux*muy - mux - muy )
  HSVariance <- (2*(n-4)*(n-5)/(n*(n-1)*(n-2)*(n-3))) * sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Ux%*%MSx))%*%t(Ux) )) * sum(diag( Uy%*%((MSy%*%t(Uy))%*%(Uy%*%MSy))%*%t(Uy) ))/n^4
  alpha <- (HSMean)^2/HSVariance
  beta  <- HSVariance/(HSMean)
  pval <- 1-pgamma(q = hsic, shape = alpha, rate = 1/beta)

  names(hsic) <- "HSIC"
  names(HSMean) <-"HSIC mean"
  names(HSVariance) <- "HSIC variance"
  dataname <- paste("Gamma approximation", sep = "")
  e <- list(method = paste("HSIC test of independence", sep = ""),
            statistic = hsic,
            estimate = hsic,
            estimates = c(hsic,HSMean,HSVariance),
            p.value = pval,
            replicates = NULL,
            data.name = dataname)
  class(e) <- "htest"
  return(e)

}
