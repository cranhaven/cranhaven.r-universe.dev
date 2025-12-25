#' Sample discrete Markov chains
#'
#' This function draws independent random samples of a discrete Markov chain.
#'
#' @param pInit an array of length K, containing the marginal distribution of the states for the first variable.
#' @param Q an array of size (p-1,K,K), containing a list of p-1 transition matrices between the K states of the Markov chain.
#' @param n the number of independent samples to be drawn (default: 1).
#' @return A matrix of size n-by-p containing the n observed Markov chains of length p.
#'
#' @family models
#'
#' @details
#' Each element of the output matrix is an integer value between 0 and K-1.
#' The transition matrices contained in Q are defined such that \eqn{P[X_{j+1}=k|X_{j}=l]=Q[j,l,k]}.
#'
#' @references
#'   \insertRef{sesia2019}{SNPknock}
#'
#' @examples
#' p=10; K=5;
#' pInit = rep(1/K,K)
#' Q = array(stats::runif((p-1)*K*K),c(p-1,K,K))
#' for(j in 1:(p-1)) { Q[j,,] = Q[j,,] / rowSums(Q[j,,]) }
#' X = sampleDMC(pInit, Q, n=20)
#'
#' @export
sampleDMC <- function(pInit, Q, n=1) {
  # Verify dimensions
  stopifnot(length(pInit)==dim(Q)[2])
  stopifnot(dim(Q)[2]==dim(Q)[3])

  # Initialize the Markov chain
  p = dim(Q)[1]+1
  chain = array(0, c(n,p))

  # Sample the Markov chain
  W = t(matrix(rep(pInit,n),length(pInit)))
  chain[,1] = rand_weighted(W)
  for(j in 2:p) {
    W = matrix(Q[j-1,chain[,j-1],],n)
    chain[,j] = rand_weighted(W)
  }
  chain = chain - 1
  storage.mode(chain) = "integer"
  return(chain)
}

#' Sample hidden Markov models
#'
#' This function draws independent random samples of an hidden Markov model.
#'
#' @param pInit an array of length K, containing the marginal distribution of the states for the first variable.
#' @param Q an array of size (p-1,K,K), containing a list of p-1 transition matrices between the K states of the Markov chain.
#' @param pEmit an array of size (p,M,K), containing the emission probabilities for each of the M possible emission states,
#'              from each of the K hidden states and the p variables.
#' @param n the number of independent samples to be drawn (default: 1).
#' @return A matrix of size n-by-p containing the n observed Markov chains of length p.
#'
#' @family models
#'
#' @details
#' Each element of the output matrix is an integer value between 0 and K-1.
#' The transition matrices contained in Q are defined with the same convention as in \link{sampleDMC}.
#' The emission propability matrices contained in pEmit are defined such that \eqn{P[X_{j}=k|H_{j}=l]=\mathrm{pEmit}[j,k,l]},
#' where \eqn{H_j} is the latent variable associated to \eqn{X_j}.
#'
#' @references
#'   \insertRef{sesia2019}{SNPknock}
#'
#' @examples
#' p=10; K=5; M=3;
#' pInit = rep(1/K,K)
#' Q = array(stats::runif((p-1)*K*K),c(p-1,K,K))
#' for(j in 1:(p-1)) { Q[j,,] = Q[j,,] / rowSums(Q[j,,]) }
#' pEmit = array(stats::runif(p*M*K),c(p,M,K))
#' for(j in 1:p) { pEmit[j,,] = pEmit[j,,] / rowSums(pEmit[j,,]) }
#' X = sampleHMM(pInit, Q, pEmit, n=20)
#'
#' @export
sampleHMM <- function(pInit, Q, pEmit, n=1) {
  # Verify dimensions
  stopifnot(length(pInit)==dim(Q)[2])
  stopifnot(dim(pEmit)[3]==dim(Q)[2])
  stopifnot(dim(pEmit)[1]==dim(Q)[1]+1)
  stopifnot(dim(Q)[2]==dim(Q)[3])

  # Sample the hidden chain
  chain = sampleDMC(pInit, Q, n=n)+1

  # Sample the emissions
  p = dim(Q)[1]+1
  emissions = array(0, c(n,p))
  for(j in 1:p) {
    W = matrix(t(pEmit[j,,chain[,j]]),n)
    emissions[,j] = rand_weighted(W)
  }
  emissions = emissions-1
  storage.mode(emissions) = "integer"
  return(emissions)
}

#' Random sampling from discrete distribution
#'
#' @keywords internal
rand_weighted = function(W) {
  W_cdf = t(apply(W/rowSums(W), 1, cumsum))
  R = stats::runif(nrow(W))
  return(rowSums(W_cdf<R)+1)
}
