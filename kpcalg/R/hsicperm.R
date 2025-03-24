#' Hilber Schmidt Independence Criterion permutation test
#'
#' Test to check the independence between two variables x and y using HSIC.
#' The hsic.perm() function, uses Hilbert-Schmidt independence criterion to test for independence between
#' random variables.
#'
#' @details Let x and y be two samples of length n. Gram matrices K and L are defined as: \eqn{K_{i,j} = \exp\frac{(x_i-x_j)^2}{\sigma^2}}{K_{i,j} =exp((x_i-x_j)^2/sig^2)} and \eqn{L_{i,j} = \exp\frac{(y_i-y_j)^2}{\sigma^2}}{L_{i,j} =exp((y_i-y_j)^2/sig^2)}. \eqn{H_{i,j} = \delta_{i,j} - \frac{1}{n}}{H_{i,j} = \delta_{i,j} - 1/n}. Let \eqn{A=HKH} and \eqn{B=HLH}, then \eqn{HSIC(x,y)=\frac{1}{n^2}Tr(AB)}{HSIC(x,y)=Tr(AB)/n^2}. Permutation test permutes y p times to get \eqn{y_{(p)}} and calculates HSIC(x,y_{(p)}). \eqn{pval = \frac{1(HSIC(x,y)>HSIC(x,y_{(p)}))}{p}}{(HSIC(x,y)>HSIC(x,y_{(p)}))/p}.
#' @param x data of first sample
#' @param y data of second sample
#' @param p Number of permutations. Default is 100
#' @param numCol maximum number of columns that we use for the incomplete Cholesky decomposition
#' @param sig Gaussian kernel width for HSIC tests. Default is 1
#'
#' @importFrom kernlab inchol rbfdot
#' @importFrom stats var kmeans pgamma
#' @import methods
#'
#' @references A. Gretton et al. (2005). Kernel Methods for Measuring Independence. JMLR 6 (2005) 2075-2129.
#'
#' @export
#' @return hsic.perm() returns a list with class htest containing
#' \item{method}{description of test}
#' \item{statistic}{observed value of the test statistic}
#' \item{estimate}{HSIC(x,y)}
#' \item{estimates}{a vector: [HSIC(x,y), mean of HSIC(x,y), variance of HSIC(x,y)]}
#' \item{replicates}{replicates of the test statistic}
#' \item{p.value}{approximate p-value of the test}
#' \item{data.name}{desciption of data}
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk}) and Nina Ines Bertille Desgranges
#' @seealso \link{hsic.gamma}, \link{hsic.clust}, \link{kernelCItest}
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



hsic.perm <- function(   x, # first variable
                         y, # second variable
                         sig=1, #sigma for the Gaussian kernel
                         p = 100, # number of permutation in finding p-value
                         numCol=50 # number of columns for incomplete Cholesky decomposition
                         ){
  n <- length(x)
  H <- diag(n)-matrix(1/n,n,n)
  rbf <- rbfdot(sigma=1/sig)
  tempx <- inchol(as.matrix(x), kernel=rbf, maxiter = numCol)
  iCHx <- tempx@.Data
  a <- svd(x = H%*%iCHx)
  Ux <- a$u
  Sx <- a$d
  ##
  tempy <- inchol(as.matrix(y), kernel=rbf, maxiter = numCol)
  iCHy <- tempy@.Data
  b <- svd(x = H%*%iCHy)
  Uy <- b$u
  Sy <- b$d
  ##
  if (length(Sx) != 1){ MSx <- diag(Sx^2) }  else {MSx <- as.matrix(Sx^2)}
  if (length(Sy) != 1){ MSy <- diag(Sy^2) }  else {MSy <- as.matrix(Sy^2)}
  hsic <- sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Uy%*%MSy))%*%t(Uy) )) /n^2
  pval.perm <- vector(length = p)
  for( i in 1:p)
  {
    perm <- sample(n)
    pval.perm[i] <- sum(diag( Ux%*%((MSx%*%t(Ux))%*%(Uy[perm,]%*%MSy))%*%t(Uy[perm,]) )) /n^2
  }
  pval <- mean(c(pval.perm,hsic)>=hsic)

  HSMean <- mean(pval.perm)
  HSVariance <- var(pval.perm)

  names(hsic) <- "HSIC"
  names(HSMean) <-"HSIC mean"
  names(HSVariance) <- "HSIC variance"
  dataname <- paste("Permutation approximation", sep = "")
  e <- list(method = paste("HSIC test of independence", sep = ""),
            statistic = hsic,
            estimate = hsic,
            estimates = c(hsic,HSMean,HSVariance),
            p.value = pval,
            replicates = pval.perm,
            data.name = dataname)
  class(e) <- "htest"
  return(e)

}
