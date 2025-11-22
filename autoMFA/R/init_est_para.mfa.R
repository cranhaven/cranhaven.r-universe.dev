init_est_para.mfa <- function (Y, g, q, start, sigma_type = "common", D_type = "common", 
          ...) 
{
  #Based on function from package EMMIXmfa with the same name.
  #See package EMMIXmfa for more details. 
  p <- ncol(Y)
  n <- nrow(Y)
  B <- array(NA, c(p, q, g))
  pivec <- array(NA, c(g, 1))
  mu <- array(NA, c(p, g))
  if (D_type == "common") {
    D <- diag(diag(cov(Y)))
    Di.sqrt <- diag(sqrt(diag(D)))
    inv.Di.sqrt <- diag(1/diag(Di.sqrt))
  }
  else if (D_type == "unique") {
    D <- array(NA, c(p, p, g))
  }
  for (i in 1:g) {
    indices <- which(start == i)
    pivec[i] <- length(indices)/n
    mu[, i] <- apply(Y[indices, ], 2, mean)
    Si <- cov(Y[indices, ])
    if (D_type == "unique") {
      D[, , i] <- diag(diag(Si))
      Di.sqrt <- diag(sqrt(diag(D[, , i])))
      inv.Di.sqrt <- diag(1/diag(Di.sqrt))
    }
    eig.list <- try(eigen(inv.Di.sqrt %*% Si %*% inv.Di.sqrt), 
                    TRUE)
    if (class(eig.list) == "try-error") 
      break
    H <- eig.list$vectors
    sort.lambda <- sort(eig.list$values, decreasing = TRUE, 
                        index.return = TRUE)
    lambda <- sort.lambda$x
    ix.lambda <- sort.lambda$ix
    if (q == p) {
      sigma2 <- 0
    }
    else {
      sigma2 <- mean(lambda[(q + 1):p])
    }
    if (q == 1) {
      B[, , i] <- Di.sqrt %*% H[, ix.lambda[1:q]] %*% diag((lambda[1:q] - 
                                                              sigma2), q)
    }
    else {
      B[, , i] <- Di.sqrt %*% H[, ix.lambda[1:q]] %*% diag((lambda[1:q] - 
                                                              sigma2))
    }
  }
  if (sigma_type == "common") 
    B <- apply(B, c(1, 2), sum)/g
  model <- list(g = g, q = q, pivec = pivec, B = B, mu = mu, 
                D = D, sigma_type = sigma_type, D_type = D_type)
  class(model) <- "mfa"
  return(model)
}