#' Simulation of extremal Student generalized Pareto vectors
#' 
#' Simulation of Pareto processes associated to the max functional. The algorithm is described in section 4 of Thibaud and Opitz (2015). 
#' The Cholesky decomposition of the matrix \code{Sigma}
#' leads to samples on the unit sphere with respect to the Mahalanobis distance. 
#' An accept-reject algorithm is then used to simulate 
#' samples from the Pareto process. If \code{normalize = TRUE}, 
#' the vector is scaled by the exponent measure \eqn{\kappa} so that the maximum of the sample is greater than \eqn{\kappa}.
#' 
#' @note If \eqn{\nu>2}, an accept-reject algorithm using simulations from the angular measure on the
#'  \eqn{l_1}{l1} is at least twice as efficient. The relative efficiency of the latter is much larger for larger \eqn{\nu}. 
#'  This algorithm should therefore not be used in high dimensions as its acceptance rate
#'  is several orders of magnitude smaller than that implemented in \link[mev]{rparp}.
#' 
#' 
#' @param n sample size
#' @param Sigma a \code{d} by \code{d} correlation matrix
#' @param nu degrees of freedom parameter
#' @param normalize logical; should unit Pareto samples above \eqn{\kappa} be returned?
#' @param trunc logical; should negative components be truncated at zero? Default to \code{TRUE}.
#' @param matchol Cholesky matrix \eqn{\mathbf{A}}{A} such that \eqn{\mathbf{A}\mathbf{A}^\top = \boldsymbol{\Sigma}}{AA^t = \Sigma}. Corresponds to \code{t(chol(Sigma))}. Default to \code{NULL}, in which case the Cholesky root is computed within the function.
#' @references Thibaud, E. and T. Opitz (2015). Efficient inference and simulation for elliptical Pareto processes. Biometrika, 102(4), 855-870.
#' @author Emeric Thibaud, Leo Belzile
#' @return an \code{n} by \code{d} matrix of samples, with \code{attributes} \code{"accept.rate"} indicating 
#' the fraction of samples accepted.
#' @export
#' @seealso \link[mev]{rparp}
#' @importFrom stats cov2cor rnorm runif
#' @examples 
#' loc <- expand.grid(1:4, 1:4)
#' Sigma <- exp(-as.matrix(dist(loc))^1.5)
#' rExtremalStudentParetoProcess(100, Sigma, nu = 2)
rExtremalStudentParetoProcess <- function(n, Sigma, nu, normalize = FALSE, matchol = NULL, trunc = TRUE){
  d <- nrow(Sigma)
  # Check the input is indeed a correlation matrix
  if(!isTRUE(all.equal(as.vector(diag(Sigma)), rep(1, d)))){
    if(all(c(isSymmetric(Sigma), eigen(Sigma, only.values = TRUE)$eigen > 1e-10))){
      warning("`Sigma` is not a corrrelation matrix.")
      Sigma <- cov2cor(Sigma)
    } else{
      stop("The correlation function provided by the user is not valid.")
    }
  }
  if (!is.null(matchol)){
    A <- matchol
  } else {
    A <- t(chol(Sigma))
  }
  
  # Container for simulations
  P <- matrix(0, nrow = n, ncol = d)
  #Compute the exponent measure
  if (normalize){
    genVec <- mvPot::genVecQMC(p = 499, d - 1)
    kap <- sum(sapply(1:d, function(j){mvPot::mvTProbQuasiMonteCarlo(p = genVec$primeP,
                                                  upperBound = 1 - Sigma[-j,j],
                                                  cov = (Sigma[-j, -j] - Sigma[-j, j, drop = FALSE] %*% Sigma[j, -j, drop = FALSE]) / (nu + 1),
                                                  nu = nu + 1, genVec = genVec$genVec)[1]}))
  } else {
    kap <- 1
  }
  #Simulation algorithm (Theorem 5.3 in Thomas Opitz's thesis, p. 80)
  N <- n # Number of simulations
  ntotsim <- 0L
  ntotacc <- 0L
  ni <- 0L # Number of remaining simulations
  while(ni < n){
    R <- runif(N)^(-1/nu) # nu Pareto variates
    V <- matrix(rnorm(N * d), ncol = d) # Standard Normal variates
    normV <- sqrt(rowSums(V^2)) # Compute L2 norm
    U <- V / normV # Create uniform on sphere
    W <- R * t(A %*% t(U)) 
    In <- apply(W, 1, max) > 1 # Check accept-reject condition
    acc <- sum(In) # Number of simulations accepted
    ntotsim <- ntotsim + N
    ntotacc <- ntotacc + acc
      if(acc > 0){
        P[(ni + 1):min(n, ni + acc), ] <- W[which(In)[1:(min(n - ni, acc))], ]
        ni <- ni + acc;
        N <- min(1e6, max(5, ceiling((n - ni) * N / acc))) #Number of simulations for next rounds
      }
  }
  if(trunc){
    P <- t(apply(P, 1, function(x){pmax(0, x)^nu }))
  } else{
    P <- sign(P) * abs(P)^nu 
  }
    samp <- kap * P
    attr(samp, "accept.rate") <- ntotacc/ntotsim
   return(samp)
}

