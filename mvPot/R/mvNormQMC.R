#' Multivariate normal distribution function
#'
#' Estimate the multivariate distribution function with quasi-Monte Carlo method.
#'
#' The function uses a quasi-Monte Carlo procedure based on randomly shifted
#' lattice rules to estimate the distribution function a multivariate normal distribution
#' as described in Genz and Bretz (2009) on page 50.
#'
#' @param p Number of samples used for quasi-Monte Carlo estimation. Must be a prime number.
#' @param upperBound Vector of probabilities, i.e., the upper bound of the integral.
#' @param cov Covariance matrix of the multivariate normal distribution. Must be positive semi-definite.
#' WARNING: for performance in high-dimensions, no check is performed on the matrix. It is the user responsibility to ensure
#' that the matrix is positive semi-definite.
#' @param ... Additional arguments passed to Cpp routine.
#' @references Genz, A. and Bretz, F. (2009). Computations of Multivariate Normal and t Probabilities, volume 105. Springer: Dordrecht.
#' @references Genz, A. (2013). QSILATMVNV \url{http://www.math.wsu.edu/faculty/genz/software/software.html}
#' @param genVec Generating vector for the quasi-Monte Carlo procedure. Can be computed using \code{genVecQMC}.
#' @return A named vector with components estimate \code{estimate} of the distribution function 
#' along \code{error}, 3 times the empirical Monte Carlo standard error over the \code{nrep} replications.
#' @examples
#'
#' #Define locations
#' loc <- expand.grid(1:4, 1:4)
#' ref <- sample.int(16, 1)
#'
#' #Compute variogram matrix
#' variogramMatrix <- ((sqrt((outer(loc[,1],loc[,1],"-"))^2 +
#' (outer(loc[,2],loc[,2],"-"))^2)) / 2)^(1.5)
#'
#' #Define an upper boud
#' upperBound <- variogramMatrix[-ref,ref]
#'
#' #Compute covariance matrix
#' cov <-  (variogramMatrix[-ref,ref]%*%t(matrix(1, (nrow(loc) - 1), 1)) +
#' t(variogramMatrix[ref,-ref]%*%t(matrix(1, (nrow(loc) - 1), 1))) -
#' variogramMatrix[-ref,-ref])
#'
#' #Compute generating vector
#' p <- 499
#' latticeRule <- genVecQMC(p, (nrow(loc) - 1))
#'
#' #Estimate the multivariate distribution function
#' mvtNormQuasiMonteCarlo(latticeRule$primeP, upperBound, cov, latticeRule$genVec)
#' @export
#' @useDynLib mvPot mvtNormCpp
mvtNormQuasiMonteCarlo = function(p, upperBound, cov, genVec, ...){
  if(missing(p) && missing(genVec)){
    p <- 499L
    genVec <- genVecQMC(p, nrow(cov))$genVec
  }
  if(!is.numeric(p) | length(p) > 1) {
    stop('p must be a prime number.')
  }

  if(!is.numeric(upperBound) | !is.vector(upperBound)) {
    stop('upperBound must be a numeric vector.')
  }

  if(!is.numeric(cov) | !is.matrix(cov)) {
    stop('upperBound must be a numeric matrix')
  }

  if(!isTRUE(all(dim(cov) == length(upperBound)))){
    stop('upperBound and cov must have the same dimension')
  }

  if(!is.numeric(genVec) | !is.vector(genVec) | length(upperBound) != length(genVec)){
    stop('genVec must be a numeric vector of the same size as upperBound')
  }
  ellipsis <- list(...)
  if(!is.null(ellipsis$nrep)){
    nrep <- as.integer(ellipsis$nrep)
    #number of Monte-Carlo replications over which to average calculations to estimate error. Default to 10.
  } else{ 
    nrep <- 10L
  }
  if(!is.null(ellipsis$antithetic)){
    antithetic <-  ellipsis$antithetic
    #should antithetic variables be used (in which case sample size is \code{2p}).
    stopifnot(is.logical(antithetic))
  } else{
    antithetic <- FALSE 
  }
  tmp <-.C(mvtNormCpp,
           as.integer(p),
           as.integer(length(upperBound)),
           as.double(cov),
           as.double(upperBound),
           as.double(genVec),
           as.integer(nrep),
           as.integer(antithetic),
           est = double(length=1),
           err = double(length=1),
           PACKAGE = "mvPot"
  )

  c(estimate = tmp$est, error = tmp$err)
}



.onUnload <- function (libpath) {
  library.dynam.unload("mvPot", libpath)
}
