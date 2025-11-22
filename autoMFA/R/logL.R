
logL <- function (Y, g, pivec, B, mu, D, numFactors)
{   #better computation of log-likelihood of MFA model
  if (!is.matrix(Y))
    Y <- as.matrix(Y)
  p <- ncol(Y)
  n <- nrow(Y)
  Fji <- array(NA, c(n, g))
  factorCount <- 0
  for (i in 1:g) {
      Bi <- matrix(B[, (factorCount + 1):(factorCount + numFactors[i])], ncol = numFactors[i])
      inv_D <- diag(1/as.vector(D[,i]))
      B_inv_D <- Bi * diag(inv_D)
      inv_O <- try(chol.inv(diag(numFactors[i]) + t(B_inv_D) %*%
                                Bi))
      if (any(class(inv_O) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[,",
                                 i, "]"))
      inv_S <- try(inv_D - B_inv_D %*% inv_O %*% t(B_inv_D))
        if (any(class(inv_S) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[,",
                                 i, "]"))
        logdetS <- sum(log(D[,i])) - log(det(inv_O))
        mahal_dist <- stats::mahalanobis(x = Y, center = mu[,
                                                     i, drop = FALSE], cov = inv_S, inverted = TRUE)
        Fji[, i] <- -0.5 * mahal_dist - (p/2) * log(2 *
                                                      pi) - 0.5 * logdetS
        factorCount = factorCount + numFactors[i]
      }
  Fji <- sweep(Fji, 2, log(pivec), "+")
  Fjmax <- apply(Fji, 1, max)
  Fji <- sweep(Fji, 1, Fjmax, "-")
  loglike <- sum(Fjmax, log(rowSums(exp(Fji))))
  Fji <- exp(Fji)
  tau <- sweep(Fji, 1, rowSums(Fji), "/")
  return(list(logL = loglike, tau = tau))
}
