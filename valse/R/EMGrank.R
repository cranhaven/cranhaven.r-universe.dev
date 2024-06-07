#' EMGrank
#'
#' Run an generalized EM algorithm developped for mixture of Gaussian regression
#' models with variable selection by an extension of the low rank estimator.
#' Reparametrization is done to ensure invariance by homothetic transformation.
#' It returns a collection of models, varying the number of clusters and the rank of the regression mean.
#'
#' @param Pi An initialization for pi
#' @param Rho An initialization for rho, the variance parameter
#' @param mini integer, minimum number of iterations in the EM algorithm, by default = 10
#' @param maxi integer, maximum number of iterations in the EM algorithm, by default = 100
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param eps real, threshold to say the EM algorithm converges, by default = 1e-4
#' @param rank vector of possible ranks
#' @param fast boolean to enable or not the C function call
#'
#' @return A list (corresponding to the model collection) defined by (phi,LLF):
#'   phi : regression mean for each cluster, an array of size p*m*k
#'   LLF : log likelihood with respect to the training set
#'
#' @export
EMGrank <- function(Pi, Rho, mini, maxi, X, Y, eps, rank, fast)
{
  if (!fast)
  {
    # Function in R
    return(.EMGrank_R(Pi, Rho, mini, maxi, X, Y, eps, rank))
  }

  # Function in C
  .Call("EMGrank", Pi, Rho, mini, maxi, X, Y, eps, as.integer(rank), PACKAGE = "valse")
}

# helper to always have matrices as arg (TODO: put this elsewhere? improve?)  -->
# Yes, we should use by-columns storage everywhere... [later!]
matricize <- function(X)
{
  if (!is.matrix(X))
    return(t(as.matrix(X)))
  X
}

# R version - slow but easy to read
.EMGrank_R <- function(Pi, Rho, mini, maxi, X, Y, eps, rank)
{
  # matrix dimensions
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)
  k <- length(Pi)

  # init outputs
  phi <- array(0, dim = c(p, m, k))
  Z <- rep(1, n)
  LLF <- 0

  # local variables
  Phi <- array(0, dim = c(p, m, k))
  deltaPhi <- c()
  sumDeltaPhi <- 0
  deltaPhiBufferSize <- 20
  
  # main loop
  ite <- 1
  while (ite <= mini || (ite <= maxi && sumDeltaPhi > eps))
  {
    # M step: update for Beta ( and then phi)
    for (r in 1:k)
    {
      Z_indice <- seq_len(n)[Z == r] #indices where Z == r
      if (length(Z_indice) == 0)
        next
      # U,S,V = SVD of (t(Xr)Xr)^{-1} * t(Xr) * Yr
      s <- svd(MASS::ginv(crossprod(matricize(X[Z_indice, ]))) %*%
               crossprod(matricize(X[Z_indice, ]), matricize(Y[Z_indice, ])))
      S <- s$d
      # Set m-rank(r) singular values to zero, and recompose best rank(r) approximation
      # of the initial product
      if (rank[r] < length(S))
        S[(rank[r] + 1):length(S)] <- 0
      phi[, , r] <- s$u %*% diag(S) %*% t(s$v) %*% Rho[, , r]
    }

    # Step E and computation of the loglikelihood
    sumLogLLF2 <- 0
    for (i in seq_len(n))
    {
      sumLLF1 <- 0
      maxLogGamIR <- -Inf
      for (r in seq_len(k))
      {
        dotProduct <- tcrossprod(Y[i, ] %*% Rho[, , r] - X[i, ] %*% phi[, , r])
        logGamIR <- log(Pi[r]) + log(gdet(Rho[, , r])) - 0.5 * dotProduct
        # Z[i] = index of max (gam[i,])
        if (logGamIR > maxLogGamIR)
        {
          Z[i] <- r
          maxLogGamIR <- logGamIR
        }
        sumLLF1 <- sumLLF1 + exp(logGamIR)/(2 * pi)^(m/2)
      }
      sumLogLLF2 <- sumLogLLF2 + log(sumLLF1)
    }

    LLF <- -1/n * sumLogLLF2

    # update distance parameter to check algorithm convergence (delta(phi, Phi))
    deltaPhi <- c(deltaPhi, max((abs(phi - Phi))/(1 + abs(phi)))) #TODO: explain?
    if (length(deltaPhi) > deltaPhiBufferSize)
      deltaPhi <- deltaPhi[2:length(deltaPhi)]
    sumDeltaPhi <- sum(abs(deltaPhi))

    # update other local variables
    Phi <- phi
    ite <- ite + 1
  }
  list(phi = phi, LLF = LLF)
}
