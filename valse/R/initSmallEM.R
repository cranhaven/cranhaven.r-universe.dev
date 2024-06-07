#' initSmallEM
#'
#' initialization of the EM algorithm
#'
#' @param k number of components
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param fast boolean to enable or not the C function call
#'
#' @return a list with phiInit (the regression parameter reparametrized),
#' rhoInit (the covariance parameter reparametrized), piInit (the proportion parameter is the
#' mixture model), gamInit (the conditional expectation)
#'
#' @importFrom stats cutree dist hclust runif
#'
#' @export
initSmallEM <- function(k, X, Y, fast)
{
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)
  nIte <- 20
  Zinit1 <- array(0, dim = c(n, nIte))
  betaInit1 <- array(0, dim = c(p, m, k, nIte))
  sigmaInit1 <- array(0, dim = c(m, m, k, nIte))
  phiInit1 <- array(0, dim = c(p, m, k, nIte))
  rhoInit1 <- array(0, dim = c(m, m, k, nIte))
  Gam <- matrix(0, n, k)
  piInit1 <- matrix(0, nIte, k)
  gamInit1 <- array(0, dim = c(n, k, nIte))
  LLFinit1 <- list()

  # require(MASS) #Moore-Penrose generalized inverse of matrix
  for (repet in 1:nIte)
  {
    distance_clus <- dist(cbind(X, Y))
    tree_hier <- hclust(distance_clus)
    Zinit1[, repet] <- cutree(tree_hier, k)

    for (r in 1:k)
    {
      Z <- Zinit1[, repet]
      Z_indice <- seq_len(n)[Z == r]  #renvoit les indices ou Z==r
      if (length(Z_indice) == 1) {
        betaInit1[, , r, repet] <- MASS::ginv(crossprod(t(X[Z_indice, ]))) %*%
          crossprod(t(X[Z_indice, ]), Y[Z_indice, ])
      } else {
        betaInit1[, , r, repet] <- MASS::ginv(crossprod(X[Z_indice, ])) %*%
          crossprod(X[Z_indice, ], Y[Z_indice, ])
      }
      sigmaInit1[, , r, repet] <- diag(m)
      phiInit1[, , r, repet] <- betaInit1[, , r, repet]  #/ sigmaInit1[,,r,repet]
      rhoInit1[, , r, repet] <- solve(sigmaInit1[, , r, repet])
      piInit1[repet, r] <- mean(Z == r)
    }

    for (i in 1:n)
    {
      for (r in 1:k)
      {
        dotProduct <- tcrossprod(Y[i, ] %*% rhoInit1[, , r, repet]
          - X[i, ] %*% phiInit1[, , r, repet])
        Gam[i, r] <- piInit1[repet, r] *
          det(rhoInit1[, , r, repet]) * exp(-0.5 * dotProduct)
      }
      sumGamI <- sum(Gam[i, ])
      # TODO: next line is a division by zero if dotProduct is big
      gamInit1[i, , repet] <- Gam[i, ]/sumGamI
    }

    miniInit <- 10
    maxiInit <- 11

    init_EMG <- EMGLLF(phiInit1[, , , repet], rhoInit1[, , , repet], piInit1[repet, ],
      gamInit1[, , repet], miniInit, maxiInit, gamma = 1, lambda = 0, X, Y,
      eps = 1e-04, fast)
    LLFinit1[[repet]] <- init_EMG$llh
  }
  b <- which.min(LLFinit1)
  phiInit <- phiInit1[, , , b]
  rhoInit <- rhoInit1[, , , b]
  piInit <- piInit1[b, ]
  gamInit <- gamInit1[, , b]

  list(phiInit = phiInit, rhoInit = rhoInit, piInit = piInit, gamInit = gamInit)
}
