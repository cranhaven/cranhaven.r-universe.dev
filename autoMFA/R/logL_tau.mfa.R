chol.inv <- function (x, ...)
{
  #From package EMMIXmfa. See package EMMIXmfa for more details. 
  C <- chol(x)
  inv_x <- chol2inv(C)
  return(inv_x)
}


logL_tau.mfa <- function (Y, g, q, pivec, B, mu, D, sigma_type, D_type, ...)
{
  #From package EMMIXmfa. See package EMMIXmfa for more details. 
  if (!is.matrix(Y))
    Y <- as.matrix(Y)
  p <- ncol(Y)
  n <- nrow(Y)
  Fji <- array(NA, c(n, g))
  if (sigma_type == "common") {
    if (D_type == "common") {
      inv_D <- diag(1/diag(D))
      B_inv_D <- B * diag(inv_D)
      inv_O <- try(chol.inv(diag(q) + t(B_inv_D) %*% B))
      if (any(class(inv_O) %in% "try-error"))
        return(loglike = paste("ill-conditioned or singular Sigma"))
      inv_S <- try(inv_D - B_inv_D %*% inv_O %*% t(B_inv_D))
      if (any(class(inv_S) %in% "try-error"))
        return(loglike = paste("ill-conditioned or singular Sigma"))
      for (i in 1:g) {
        logdetS <- sum(log(diag(D))) - log(det(inv_O))
        mahal_dist <- mahalanobis(x = Y, center = mu[,
                                                     i, drop = FALSE], cov = inv_S, inverted = TRUE)
        Fji[, i] <- -0.5 * mahal_dist - (p/2) * log(2 *
                                                      pi) - 0.5 * logdetS
      }
    }
  }
  else {
    if (D_type == "common") {
      inv_D <- diag(1/diag(D))
      for (i in 1:g) {
        B_inv_D <- B[, , i] * diag(inv_D)
        inv_O <- try(chol.inv(diag(q) + t(B_inv_D) %*%
                                B[, , i]))
        if (any(class(inv_O) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[, ",
                                 i, "]"))
        inv_S <- try(inv_D - B_inv_D %*% inv_O %*% t(B_inv_D))
        if (any(class(inv_S) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[, ",
                                 i, "]"))
        logdetS <- sum(log(diag(D))) - log(det(inv_O))
        mahal_dist <- mahalanobis(x = Y, center = mu[,
                                                     i, drop = FALSE], cov = inv_S, inverted = TRUE)
        Fji[, i] <- -0.5 * mahal_dist - (p/2) * log(2 *
                                                      pi) - 0.5 * logdetS
      }
    }
    if (D_type == "unique") {
      for (i in 1:g) {
        inv_D <- diag(1/diag(D[, , i]))
        B_inv_D <- B[, , i] * diag(inv_D)
        inv_O <- try(chol.inv(diag(q) + t(B_inv_D) %*%
                                B[, , i]))
        if (any(class(inv_O) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[,",
                                 i, "]"))
        inv_S <- try(inv_D - B_inv_D %*% inv_O %*% t(B_inv_D))
        if (any(class(inv_S) %in% "try-error"))
          return(loglike = paste("ill-conditioned or singular Sigma[,",
                                 i, "]"))
        logdetS <- sum(log(diag(D[, , i]))) - log(det(inv_O))
        mahal_dist <- mahalanobis(x = Y, center = mu[,
                                                     i, drop = FALSE], cov = inv_S, inverted = TRUE)
        Fji[, i] <- -0.5 * mahal_dist - (p/2) * log(2 *
                                                      pi) - 0.5 * logdetS
      }
    }
  }
  Fji <- sweep(Fji, 2, log(pivec), "+")
  Fjmax <- apply(Fji, 1, max)
  Fji <- sweep(Fji, 1, Fjmax, "-")
  loglike <- sum(Fjmax, log(rowSums(exp(Fji))))
  Fji <- exp(Fji)
  tau <- sweep(Fji, 1, rowSums(Fji), "/")
  return(list(logL = loglike, tau = tau))
}
