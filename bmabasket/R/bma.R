




#' Compute number of models
#' 
#' Given a basket size and maximal number of distinct
#' response rates, compute the number of possible models
#' 
#' @param K positive integer giving number of baskets
#' @param P positive integer giving maximal number of distinct rates
#' 
#' @return integer giving number of possible models
#' @examples
#' numModels(10, 10)
#' 
#' @export
numModels <- function(K, P) {
  if ( K %% 1 != 0 | K < 0 ) {
    stop('K must be a positive integer')
  }
  if ( P %% 1 != 0 | P < 0) {
    stop('P must be a positive integer')
  }
  if ( P > K ) {
    stop('P must be less than K')
  }
  numModels_cpp(K, P)
}

#' Compute posterior model probabilities
#' 
#' Given data and hyperparameters, computes posterior model probabilities
#' 
#' @importFrom partitions setparts restrictedparts
#' 
#' @param pi0             scalar or vector whose elements are between 0 and 1 giving threshold for the hypothesis test. If a scalar is provided, assumes same threshold for each basket
#' @param y               vector of responses
#' @param n               vector of sample sizes
#' @param P               integer giving maximum number of distinct parameters; default is all possible models
#' @param mu0             prior mean for beta prior
#' @param phi0            prior dispersion for beta prior
#' @param priorModelProbs (optional) vector giving prior for models. Default is proportional to \code{exp(pmp0 * D)}, where \code{D} is the number of distinct parameters in the model
#' @param pmp0            nonnegative scalar. Value of 0 corresponds to uniform prior across model space. Ignored if priorModelProbs is specified
#' 
#' @return a list with the following structure:
#' 
#' \describe{
#'   \item{bmaProbs}{model-averaged probabilities that each basket is larger than \code{pi0}}
#'   \item{bmaMeans}{model-averaged posterior mean for each basket}
#' }
#' 
#' @examples
#' ## Simulate data with 3 baskets
#' probs <- c(0.5, 0.25, 0.25)
#' n <- rep(100, length(probs))
#' y <- rbinom(length(probs), size = n, prob = probs)
#' bma(0.5, y, n)
#' 
#' @export
bma <- function( 
  pi0, y, n, P = NULL, mu0 = 0.5, phi0 = 1,
  priorModelProbs = NULL, pmp0 = 1
  ) {
  
  if ( length(pi0) == 1 ) {
    pi0 <- rep(pi0, times = length(y) )
  }
  
  if ( length(y) != length(n) ) {
    stop('y and n must have equal length')
  }
  if ( any( y > n ) ) {
    stop('all values in y must be no larger than corresponding values in n')
  }
  
  ## Check prior parameters
  if ( mu0 <= 0 | mu0 >= 1 ) {
    stop('prior mean hyperparameter mu0 must be between 0 and 1')
  }
  if ( phi0 <= 0 ) {
    stop('prior dispersion hyperparameter p0 must be positive')
  }
  
  ## Default value of P is K = number of baskets
  K <- length(y)
  if ( is.null(P) ) {
    P <- K
  }
  if ( P > K ) {
    stop(
      paste('The number of baskets is', K, 'which must be less than max number of distinct probabilities P =', P)
    )
  }
  
  ## Get matrix of indices for partition and number of models
  parts <- as.matrix( setparts( restrictedparts(K, P) ) ) - 1 ## -1 for c++ indexing
  J     <- ncol(parts)
    
  ## default prior model prob is # sets in partition ^ pmp0
  if ( is.null(priorModelProbs ) ) {
    priorModelProbs <- do.call('pmax', c(as.data.frame(t(parts)),na.rm=TRUE) ) + 1
    priorModelProbs <- exp(pmp0 * priorModelProbs)
    priorModelProbs <- priorModelProbs / sum(priorModelProbs)
  }
  
  if ( sum(priorModelProbs) != 1 ) {
    warning('Normalizing priorModelProbs because elements did not sum to 1')
    priorModelProbs = priorModelProbs / sum(priorModelProbs)
  }
  
  if ( length(priorModelProbs) != J ) {
    stop('length of priorModelProbs must be equal to number of models. Use numModels function to compute this.')
  }
  
  
  ## Create matrix of data
  datMat <- cbind( y, n-y )
  
  ## Return C++ function
  bma_cpp(pi0, datMat, parts, mu0, phi0, log(priorModelProbs))
}

