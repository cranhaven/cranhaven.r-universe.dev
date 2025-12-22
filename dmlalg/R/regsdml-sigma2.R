# variance for DML method
sigma2_DML <- function(all_residuals, betahat) {
  n <- length(all_residuals[[1]]$rY)
  d <- nrow(as.matrix(betahat))
  q <- ncol(all_residuals[[1]]$rA)
  K <- length(all_residuals)

  Jzerohat <- matrix(0, nrow = d, ncol = q)
  cov_loss <- matrix(0, nrow = q, ncol = q)

  for (k in seq_len(K)) {
    mat_1 <- crossprod(all_residuals[[k]]$rA, all_residuals[[k]]$rX) / n
    mat_2_inv_mat_1 <-
      qr.solve(crossprod(all_residuals[[k]]$rA,  all_residuals[[k]]$rA) / n, mat_1)
    Jzerohat <-
      Jzerohat + qr.solve(crossprod(mat_2_inv_mat_1, mat_1), t(mat_2_inv_mat_1))

    loss <- sweep(all_residuals[[k]]$rA,
                  1,
                  all_residuals[[k]]$rY - all_residuals[[k]]$rX %*% betahat,
                  FUN = "*")
    cov_loss <- cov_loss + crossprod(loss, loss) / n
  }
  Jzerohat <- Jzerohat / K

  Jzerohat %*% tcrossprod(cov_loss / K, Jzerohat) / (n * K) # = sigma2
}

# more stable version of sigma2_DML: output a warning message if this
# method needs to be employed
sigma2_DML_stable <- function(all_residuals, betahat) {
  n <- length(all_residuals[[1]]$rY)
  d <- nrow(as.matrix(betahat))
  q <- ncol(all_residuals[[1]]$rA)
  K <- length(all_residuals)

  cov_loss <- matrix(0, nrow = q, ncol = q)
  mat_2_full <- matrix(0, nrow = q, ncol = q)
  mat_1_full <- matrix(0, nrow = d, ncol = q)

  for (k in seq_len(K)) {
    mat_1_full <- mat_1_full + crossprod(all_residuals[[k]]$rX, all_residuals[[k]]$rA) / n
    mat_2_full <- mat_2_full + crossprod(all_residuals[[k]]$rA, all_residuals[[k]]$rA) / n

    loss <- sweep(all_residuals[[k]]$rA,
                  1,
                  all_residuals[[k]]$rY - all_residuals[[k]]$rX %*% betahat,
                  FUN = "*")
    cov_loss <- cov_loss + crossprod(loss, loss) / n
  }
  mat_1_full <- t(mat_1_full) / K
  mat_2_full <- mat_2_full / K
  mat_2_full_inf_mat_1_full <- qr.solve(mat_2_full, mat_1_full)
  Jzerohat <- qr.solve(crossprod(mat_2_full_inf_mat_1_full, mat_1_full),
                       t(mat_2_full_inf_mat_1_full))

  warning("Essentially perfect fit: DML summary may be unreliable.")

  Jzerohat %*% tcrossprod(cov_loss / K, Jzerohat) / (n * K)
}

# variance for regularized methods
sigma2_gamma <- function(all_residuals, betahat, gamma) {
  n <- length(all_residuals[[1]]$rY)
  d <- length(betahat)
  q <- ncol(all_residuals[[1]]$rA)
  K <- length(all_residuals)

  D1 <- matrix(0, nrow = d, ncol = d)
  D2 <- matrix(0, nrow = d, ncol = d)
  D4 <- matrix(0, nrow = d, ncol = d)

  for (k in seq_len(K)) {
    res <- all_residuals[[k]]$rY - all_residuals[[k]]$rX %*% betahat
    losstilde <- sweep(all_residuals[[k]]$rX, 1, res, FUN = "*")

    loss <- sweep(all_residuals[[k]]$rA, 1, res, FUN = "*")
    loss_mean <- colSums(loss) / n

    loss1 <- array(apply(cbind(all_residuals[[k]]$rX, all_residuals[[k]]$rA), 1,
                         function(x) outer(x[seq_len(d)], x[(d + 1):(d + q)])),
                   dim = c(d, q, n))
    loss1_mean <- apply(loss1, c(1, 2), mean)

    loss2 <- t(apply(as.matrix(all_residuals[[k]]$rA), 1, function(x) crossprod(rbind(x))))
    loss2_mean <- if (q == 1) {
      mean(loss2)
    } else {
      colSums(loss2) / n
    }
    loss2_mean_inv <- qr.solve(matrix(loss2_mean, nrow = q, ncol = q))

    loss3_mean <- crossprod(all_residuals[[k]]$rX, all_residuals[[k]]$rX) / n

    D1 <- D1 + loss3_mean
    D2 <- D2 +
      rbind(loss1_mean, deparse.level = 0) %*%
      tcrossprod(loss2_mean_inv, rbind(loss1_mean, deparse.level = 0))

    D3 <- rbind(loss1_mean, deparse.level = 0) %*% loss2_mean_inv
    D5 <- loss2_mean_inv %*% cbind(loss_mean, deparse.level = 0)

    mu <- gamma - 1

    lossBarPrime <- losstilde + mu * tcrossprod(loss, D3) +
      mu * if (q >= 2) {
        intermediate <- sweep(sweep(loss2, 2, loss2_mean, FUN = "-"), 2,
                              rep(D5, each = q), FUN = "*")
        intermediate_summed <- matrix(0, nrow = n, ncol = q)
        for (i in seq_len(q)) {
          intermediate_summed[, i] <- rowSums(intermediate[, seq(i, q ^ 2, by = q)])
        }

        t(apply(sweep(sweep(loss1, c(1, 2), loss1_mean, FUN = "-"),
                      c(2, 3), as.vector(D5), FUN = "*"), c(1, 3), sum)) -
          tcrossprod(intermediate_summed, D3)
      } else {
        matrix(sweep(loss1, 3, as.vector(loss1_mean), FUN = "-"),
               nrow = n, byrow = TRUE) * as.vector(D5) -
          crossprod((loss2 - loss2_mean) * as.vector(D5), t(D3))
    }

    D4 <- D4 + crossprod(lossBarPrime, lossBarPrime) / n
  }

  D1 <- D1 / K
  D2 <- D2 / K
  D1plusD2inv <- qr.solve(D1 + mu * D2)
  D4 <- D4 / K

  D1plusD2inv %*% tcrossprod(D4, D1plusD2inv) / (n * K) # = sigmahat2.gamma
}
