#' EMGLLF
#'
#' Run a generalized EM algorithm developped for mixture of Gaussian regression
#' models with variable selection by an extension of the Lasso estimator (regularization parameter lambda).
#' Reparametrization is done to ensure invariance by homothetic transformation.
#' It returns a collection of models, varying the number of clusters and the sparsity in the regression mean.
#'
#' @param phiInit an initialization for phi
#' @param rhoInit an initialization for rho
#' @param piInit an initialization for pi
#' @param gamInit initialization for the a posteriori probabilities
#' @param mini integer, minimum number of iterations in the EM algorithm, by default = 10
#' @param maxi integer, maximum number of iterations in the EM algorithm, by default = 100
#' @param gamma integer for the power in the penaly, by default = 1
#' @param lambda regularization parameter in the Lasso estimation
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param eps real, threshold to say the EM algorithm converges, by default = 1e-4
#' @param fast boolean to enable or not the C function call
#'
#' @return A list (corresponding to the model collection) defined by (phi,rho,pi,llh,S,affec):
#'   phi : regression mean for each cluster, an array of size p*m*k
#'   rho : variance (homothetic) for each cluster, an array of size m*m*k
#'   pi : proportion for each cluster, a vector of size k
#'   llh : log likelihood with respect to the training set
#'   S : selected variables indexes, an array of size p*m*k
#'   affec : cluster affectation for each observation (of the training set)
#'
#' @export
EMGLLF <- function(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda,
  X, Y, eps, fast)
{
  if (!fast)
  {
    # Function in R
    return(.EMGLLF_R(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda,
      X, Y, eps))
  }

  # Function in C
  .Call("EMGLLF", phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda,
    X, Y, eps, PACKAGE = "valse")
}

# R version - slow but easy to read
.EMGLLF_R <- function(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda,
  X, Y, eps)
{
  # Matrix dimensions
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)
  k <- length(piInit)

  # Adjustments required when p==1 or m==1 (var.sel. or output dim 1)
  if (p==1 || m==1)
    phiInit <- array(phiInit, dim=c(p,m,k))
  if (m==1)
    rhoInit <- array(rhoInit, dim=c(m,m,k))

  # Outputs
  phi <- phiInit
  rho <- rhoInit
  pi <- piInit
  llh <- -Inf
  S <- array(0, dim = c(p, m, k))

  # Algorithm variables
  gam <- gamInit
  Gram2 <- array(0, dim = c(p, p, k))
  ps2 <- array(0, dim = c(p, m, k))
  X2 <- array(0, dim = c(n, p, k))
  Y2 <- array(0, dim = c(n, m, k))

  for (ite in 1:maxi)
  {
    # Remember last pi,rho,phi values for exit condition in the end of loop
    Phi <- phi
    Rho <- rho
    Pi <- pi

    # Computations associated to X and Y
    for (r in 1:k)
    {
      for (mm in 1:m)
        Y2[, mm, r] <- sqrt(gam[, r]) * Y[, mm]
      for (i in 1:n)
        X2[i, , r] <- sqrt(gam[i, r]) * X[i, ]
      for (mm in 1:m)
        ps2[, mm, r] <- crossprod(X2[, , r], Y2[, mm, r])
      for (j in 1:p)
      {
        for (s in 1:p)
          Gram2[j, s, r] <- crossprod(X2[, j, r], X2[, s, r])
      }
    }

    ## M step

    # For pi
    b <- sapply(1:k, function(r) sum(abs(phi[, , r])))
    gam2 <- colSums(gam)
    a <- sum(gam %*% log(pi))

    # While the proportions are nonpositive
    kk <- 0
    pi2AllPositive <- FALSE
    while (!pi2AllPositive)
    {
      pi2 <- pi + 0.1^kk * ((1/n) * gam2 - pi)
      pi2AllPositive <- all(pi2 >= 0)
      kk <- kk + 1
    }

    # t(m) is the largest value in the grid O.1^k such that it is nonincreasing
    while (kk < 1000 && -a/n + lambda * sum(pi^gamma * b) <
      # na.rm=TRUE to handle 0*log(0)
      -sum(gam2 * log(pi2), na.rm=TRUE)/n + lambda * sum(pi2^gamma * b))
    {
      pi2 <- pi + 0.1^kk * (1/n * gam2 - pi)
      kk <- kk + 1
    }
    t <- 0.1^kk
    pi <- (pi + t * (pi2 - pi))/sum(pi + t * (pi2 - pi))

    # For phi and rho
    for (r in 1:k)
    {
      for (mm in 1:m)
      {
        ps <- 0
        for (i in 1:n)
          ps <- ps + Y2[i, mm, r] * sum(X2[i, , r] * phi[, mm, r])
        nY2 <- sum(Y2[, mm, r]^2)
        rho[mm, mm, r] <- (ps + sqrt(ps^2 + 4 * nY2 * gam2[r]))/(2 * nY2)
      }
    }

    for (r in 1:k)
    {
      for (j in 1:p)
      {
        for (mm in 1:m)
        {
          S[j, mm, r] <- -rho[mm, mm, r] * ps2[j, mm, r] +
            sum(phi[-j, mm, r] * Gram2[j, -j, r])
          if (abs(S[j, mm, r]) <= n * lambda * (pi[r]^gamma)) {
            phi[j, mm, r] <- 0
          } else if (S[j, mm, r] > n * lambda * (pi[r]^gamma)) {
            phi[j, mm, r] <- (n * lambda * (pi[r]^gamma) - S[j, mm, r])/Gram2[j, j, r]
          } else {
            phi[j, mm, r] <- -(n * lambda * (pi[r]^gamma) + S[j, mm, r])/Gram2[j, j, r]
          }
        }
      }
    }

    ## E step

    # Precompute det(rho[,,r]) for r in 1...k
    detRho <- sapply(1:k, function(r) gdet(rho[, , r]))
    sumLogLLH <- 0
    for (i in 1:n)
    {
      # Update gam[,]; use log to avoid numerical problems
      logGam <- sapply(1:k, function(r) {
        log(pi[r]) + log(detRho[r]) - 0.5 *
          sum((Y[i, ] %*% rho[, , r] - X[i, ] %*% phi[, , r])^2)
      })

      logGam <- logGam - max(logGam) #adjust without changing proportions
      gam[i, ] <- exp(logGam)
      norm_fact <- sum(gam[i, ])
      gam[i, ] <- gam[i, ] / norm_fact
      sumLogLLH <- sumLogLLH + log(norm_fact) - log((2 * base::pi)^(m/2))
    }

    sumPen <- sum(pi^gamma * b)
    last_llh <- llh
    llh <- -sumLogLLH/n #+ lambda * sumPen
    dist <- ifelse(ite == 1, llh, (llh - last_llh)/(1 + abs(llh)))
    Dist1 <- max((abs(phi - Phi))/(1 + abs(phi)))
    Dist2 <- max((abs(rho - Rho))/(1 + abs(rho)))
    Dist3 <- max((abs(pi - Pi))/(1 + abs(Pi)))
    dist2 <- max(Dist1, Dist2, Dist3)

    if (ite >= mini && (dist >= eps || dist2 >= sqrt(eps)))
      break
  }

	affec = apply(gam, 1, which.max)
  list(phi = phi, rho = rho, pi = pi, llh = llh, S = S, affec=affec)
}
