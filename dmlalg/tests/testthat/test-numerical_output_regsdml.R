params <- NULL
tol <- 0.01

####################
# helper functions
####################

cond_exp_ols_helper <- function(yy_fit, ww_fit, ww_predict) {
  s <- ncol(as.matrix(yy_fit))
  n <- nrow(as.matrix(ww_predict))
  eYgW_hat <- matrix(0, nrow = n, ncol = s)

  formula <- as.formula(paste("resp~",
                              paste(c(rbind(colnames(ww_fit), c(rep("+", length(colnames(ww_fit)) - 1), ""))), sep = "", collapse = "")))

  for (i in 1:s) {
    YgW_lm <- lm(formula, data = cbind(data.frame(resp = as.matrix(yy_fit)[, i]), ww_fit))
    eYgW_hat[, i] <- predict(YgW_lm, newdata = ww_predict)
  }
  return(eYgW_hat)
}

cond_exp_ols_old <- function(aa_fit, xx_fit, yy_fit, ww_fit, ww_predict) {
  eYgW_hat <- cond_exp_ols_helper(yy_fit, ww_fit, ww_predict)
  eXgW_hat <- cond_exp_ols_helper(xx_fit, ww_fit, ww_predict)
  eAgW_hat <- cond_exp_ols_helper(aa_fit, ww_fit, ww_predict)

  return(list(eYgW_hat = eYgW_hat, eXgW_hat = eXgW_hat, eAgW_hat = eAgW_hat))
}

beta_split_PrA_old <- function(data, I, Ic) {
  # initialize data
  aa <- data$aa
  xx <- data$xx
  yy <- data$yy
  ww <- data$ww

  # assign splitted data
  ww_fit <- ww[Ic, , drop = FALSE]
  aa_fit <- as.matrix(aa)[Ic, ]
  xx_fit <- xx[Ic]
  yy_fit <- yy[Ic]
  ww_predict <- ww[I, , drop = FALSE]
  aa_predict <- as.matrix(aa)[I, ]
  xx_predict <- xx[I]
  yy_predict <- yy[I]

  egW_hat_all <- cond_exp_ols_old(aa_fit, xx_fit, yy_fit, ww_fit, ww_predict)
  eAgW_hat <- egW_hat_all$eAgW_hat
  eXgW_hat <- egW_hat_all$eXgW_hat
  eYgW_hat <- egW_hat_all$eYgW_hat

  # compute residuals
  rA <- aa_predict - eAgW_hat
  rX <- xx_predict - eXgW_hat
  rY <- yy_predict - eYgW_hat

  # compute projected residuals
  rX_gW <- lm(rX ~ rA - 1)$fitted.values
  rY_gW <- lm(rY ~ rA - 1)$fitted.values

  return(list(rX = rX, rX_gW = rX_gW, rY = rY, rY_gW = rY_gW, rA = rA))
}

beta_split_gamma_DML2_old <- function(rX, rX_gW, rY, rY_gW, gamma) {
  n <- length(rY)
  rX_gamma <- rX + (sqrt(gamma) - 1) * rX_gW
  rY_gamma <- rY + (sqrt(gamma) - 1) * rY_gW

  mat <- t(rX_gamma) %*% rX_gamma / n
  vec <- t(rX_gamma) %*% rY_gamma / n

  return(list(mat = mat, vec = vec))
}

beta_N_old <- function(data, K, gamma, DML, get_var = FALSE) {
  xx <- data$xx
  yy <- data$yy
  n <- length(yy)
  d <- ncol(as.matrix(xx))
  stopifnot(d == 1)

  mat <- array(0, dim = c(d, d, K))
  vec <- array(0, dim = c(d, 1, K))
  mat_gamma <- array(0, dim = c(d * length(gamma), d, K))
  vec_gamma <- array(0, dim = c(d * length(gamma), 1, K))
  all_residuals <- vector(mode = "list", length = K)
  n_reorder <- sample(1:n, n, replace = FALSE)
  folds <- cut(n_reorder, breaks = K, labels = FALSE)
  for (k in 1:K) {
    I <- c(1:n)[folds == k]
    Ic <- setdiff(1:n, I)
    PrA <- beta_split_PrA_old(data, I, Ic)
    rX <- PrA$rX
    rX_gW <- PrA$rX_gW
    rY <- PrA$rY
    rY_gW <- PrA$rY_gW
    rA <- PrA$rA

    all_residuals[[k]] <- list(rX = rX, rY = rY, rA = rA)

    mat[, , k] <- t(rX_gW) %*% rX_gW / length(I)
    vec[, , k] <- t(rX_gW) %*% rY_gW / length(I)

    res_gamma <- t(sapply(gamma, beta_split_gamma_DML2_old, rX = rX, rX_gW = rX_gW, rY = rY, rY_gW = rY_gW))
    mat_gamma[, , k] <- matrix(unlist(res_gamma[, 1]), ncol = d, byrow = TRUE)
    vec_gamma[, , k] <- matrix(unlist(res_gamma[, 2]), ncol = d, byrow = TRUE)
  }

  if (DML == "DML1") {
    # DML1
    beta_k_DML1 <- matrix(0, nrow = d, ncol = K)
    for (k in 1:K) {
      beta_k_DML1[, k] <- qr.solve(mat[, , k]) %*% vec[, , k]
    }
    beta_N_DML1 <- apply(beta_k_DML1, 1, mean)

    beta_N_gamma_DML1 <- matrix(0, nrow = d, ncol = length(gamma))
    for (i in 1:K) {
      for (j in seq_len(length(gamma))) {
        beta_N_gamma_DML1[, j] <- beta_N_gamma_DML1[, j] + qr.solve(mat_gamma[j, , i]) %*% vec_gamma[j, , i]
      }
    }
    beta_N_gamma_DML1 <- beta_N_gamma_DML1 / K
  } else if (DML == "DML2") {
    # DML2
    mat_DML2 <- apply(mat, c(1:2), sum) / K
    vec_DML2 <- apply(vec, c(1:2), sum) / K
    beta_N_infty_DML2 <- qr.solve(mat_DML2, vec_DML2)

    mat_gamma_DML2 <- apply(mat_gamma, c(1, 2), sum) / K
    vec_gamma_DML2 <- apply(vec_gamma, c(1, 2), sum) / K
    beta_N_gamma_DML2 <- vec_gamma_DML2 / mat_gamma_DML2
  }

  if (DML == "DML1") {
    beta_N_infty_DML2 <- beta_N_DML1
    beta_N_gamma_DML2 <- beta_N_gamma_DML1
  }

  if (get_var == FALSE) {
    to_return <- list(beta_N_infty_DML2 = beta_N_infty_DML2, beta_N_gamma_DML2 = beta_N_gamma_DML2)
  } else {
    as_var_gamma_DML2 <- rep(0, length(gamma))
    for (i in seq_len(length(gamma))) {
      as_var_gamma_DML2[i] <- sigmahat2_gamma_old(all_residuals, betahat = beta_N_gamma_DML2[i], gamma = gamma[i])
    }
    as_var_infty_DML2 <- sigmahat2_infty_old(all_residuals = all_residuals, betahat = beta_N_infty_DML2)

    to_return <- list(beta_N_infty_DML2 = beta_N_infty_DML2, beta_N_gamma_DML2 = beta_N_gamma_DML2,
                      as_var_infty_DML2 = as_var_infty_DML2, as_var_gamma_DML2 = as_var_gamma_DML2)
  }

  return(to_return)
}

sigmahat2_infty_old <- function(all_residuals, betahat) {
  n <- length(all_residuals[[1]]$rY)
  d <- nrow(as.matrix(betahat))
  s <- ncol(as.matrix(all_residuals[[1]]$rA))
  K <- length(all_residuals)

  Jzerohat <- matrix(0, nrow = d, ncol = s)
  cov_loss <- matrix(0, nrow = s, ncol = s)
  for (k in 1:K) {
    rA <- as.matrix(all_residuals[[k]]$rA)
    rX <- as.matrix(all_residuals[[k]]$rX)
    rY <- as.matrix(all_residuals[[k]]$rY)

    mat_1 <- t(rX) %*% rA / n
    mat_2 <- qr.solve(t(rA) %*% rA / n)
    Jzerohat <- Jzerohat + qr.solve(mat_1 %*% mat_2 %*% t(mat_1)) %*% mat_1 %*% mat_2

    loss <- sweep(as.matrix(rA), 1, rY - rX %*% t(betahat), FUN = "*")
    cov_loss <- cov_loss + t(loss) %*% loss / n
  }
  Jzerohat <- Jzerohat / K
  cov_loss <- cov_loss / K

  sigmahat2_infty <- Jzerohat %*% cov_loss %*% t(Jzerohat) / (n * K)
  return(sigmahat2_infty)
}

sigmahat2_gamma_old <- function(all_residuals, betahat, gamma) {
  n <- length(all_residuals[[1]]$rY)
  d <- nrow(as.matrix(betahat))
  q <- ncol(as.matrix(all_residuals[[1]]$rA))
  K <- length(all_residuals)

  D1 <- matrix(0, nrow = d, ncol = d)
  D2 <- matrix(0, nrow = d, ncol = d)
  D4 <- matrix(0, nrow = d, ncol = d)

  for (k in 1:K) {
    rA <- as.matrix(all_residuals[[k]]$rA)
    rX <- as.matrix(all_residuals[[k]]$rX)
    rY <- as.matrix(all_residuals[[k]]$rY)

    # assume that d = 1 holds
    res <- rY - rX %*% t(betahat)
    losstilde <- sweep(as.matrix(rX), 1, res, FUN = "*")

    loss <- sweep(as.matrix(rA), 1, res, FUN = "*")
    loss_mean <- colSums(loss) / n

    loss1 <- sweep(as.matrix(rA), 1, rX, FUN = "*")
    loss1_mean <- colSums(loss1) / n

    loss2 <- t(apply(as.matrix(rA), 1, function(x) crossprod(rbind(x))))
    if (q == 1) {
      loss2_mean <- sum(loss2) / n
    } else {
      loss2_mean <- colSums(loss2) / n
    }
    loss2_mean_inv <- qr.solve(matrix(loss2_mean, nrow = q, ncol = q))

    loss3_mean <- t(rX) %*% rX / n

    D1 <- D1 + loss3_mean
    D2 <- D2 + rbind(loss1_mean, deparse.level = 0) %*% loss2_mean_inv %*% cbind(loss1_mean, deparse.level = 0)

    D3 <- rbind(loss1_mean, deparse.level = 0) %*% loss2_mean_inv
    D5 <- loss2_mean_inv %*% cbind(loss_mean, deparse.level = 0)

    mu <- gamma - 1

    if (q >= 2) {
      intermediate <- sweep(sweep(loss2, 2, loss2_mean, FUN = "-"), 2, rep(D5, each = q), FUN = "*")
      intermediate_summed <- matrix(0, nrow = n, ncol = q)
      for (i in 1:q) {
        intermediate_summed[, i] <- rowSums(intermediate[, seq(i, q ^ 2, by = q)])
      }
      lossBarPrime <- losstilde +
        mu * loss %*% t(D3) +
        mu * rowSums(sweep(sweep(loss1, 2, loss1_mean, FUN = "-"), 2, as.vector(D5), FUN = "*")) -
        mu * rowSums(sweep(intermediate_summed, 2, as.vector(D3), FUN = "*"))
    } else {
      lossBarPrime <- as.vector(losstilde +
                                  mu * loss %*% t(D3) +
                                  mu * (loss1 - loss1_mean) * as.vector(D5)) -
        mu * (loss2 - loss2_mean) * as.vector(D5) * as.vector(D3)
    }

    D4 <- D4 + sum(lossBarPrime ^ 2) / n
  }

  D1 <- D1 / K
  D2 <- D2 / K
  D1plusD2inv <- qr.solve(D1 + (gamma - 1) * D2)
  D4 <- D4 / K

  sigmahat2_gamma <- D1plusD2inv %*% D4 %*% t(D1plusD2inv) / (n * K)
  return(sigmahat2_gamma)
}


####################
# generate data
####################

set.seed(19)
n <- 40
s <- 6

ww_frame <- as.data.frame(matrix(rnorm(n * s, 0, 1), ncol = s))
ww_frameone <- as.data.frame(matrix(rnorm(n * 1, 0, 1), ncol = 1))
ww_mat <- matrix(rnorm(n * s, 0, 1), ncol = s)
ww_matone <- matrix(rnorm(n * 1, 0, 1), ncol = 1)
ww_vec <- rnorm(n, 0, 1)

yy_vec <- rnorm(n, 0, 1)
yy_frame <- as.data.frame(rnorm(n, 0, 1), ncol = 1)
yy_mat <- matrix(rnorm(n, 0, 1), ncol = 1)

aa_frame <- as.data.frame(cbind(rnorm(n, 0, 1), rnorm(n, 0, 1), rnorm(n, 0, 1)))
aa_frameone <- as.data.frame(matrix(rnorm(n, 0, 1), ncol = 1))
aa_mat <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1), rnorm(n, 0, 1))

aa_matone <- matrix(rnorm(n, 0, 1), ncol = 1)
aa_vec <- rnorm(n, 0, 1)

xx_frame <- as.data.frame(cbind(rnorm(n, 0, 1), rnorm(n, 0, 1)))
xx_frameone <- as.data.frame(matrix(rnorm(n, 0, 1), ncol = 1))
xx_mat <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1))
xx_matone <- matrix(rnorm(n, 0, 1), ncol = 1)
xx_vec <- rnorm(n, 0, 1)

aa <- aa_matone
ww <- ww_frameone
xx <- xx_matone
yy <- yy_mat
get_data_list <- function() {
  list(yy = yy, aa = aa, ww = ww, xx = xx)
}


####################
# perform tests
####################

test_that("numerical estimators of beta and sigma are correct if d = 1", {
  check_beta_output_numeric <- function(res_old, res) {
    expect_equal(as.numeric(res_old$beta_N_infty_DML2),
                 as.numeric(res$beta_DML), tolerance = tol)
    expect_equal(as.vector(res_old$beta_N_gamma_DML2),
                 as.vector(res$beta_gamma), tolerance = tol)
    expect_equal(as.numeric(res_old$as_var_infty_DML2),
                 as.numeric(res$as_var_DML), tolerance = tol)
    expect_equal(as.vector(res_old$as_var_gamma_DML2),
                 as.vector(res$as_var_gamma), tolerance = tol)
  }

  do_computations_numeric <- function(aa, ww, xx, yy) {
    get_data_list <- function() {
      list(yy = yy, aa = aa, ww = ww, xx = xx)
    }

    seed <- 5

    set.seed(seed)
    res_old <- beta_N_old(data = get_data_list(), K = 2, get_var = TRUE,
                          gamma = exp(seq(-4, 10, length.out = 20)),
                          DML = "DML2")
    set.seed(seed)
    res <- beta_crossfit(aa = aa, ww = ww, xx = xx, yy = yy, K = 2,
                         gamma = exp(seq(-4, 10, length.out = 20)),
                         DML = "DML2",
                         do_DML = TRUE,
                         do_regDML = TRUE,
                         cond_func_all = get_condexp_funcs(rep("ols", 3)),
                         params = params)
    check_beta_output_numeric(res_old, res)

    set.seed(seed)
    res_old <- beta_N_old(data = get_data_list(), K = 2, get_var = TRUE,
                          gamma = exp(seq(-4, 10, length.out = 20)),
                          DML = "DML1")
    set.seed(seed)
    res <- beta_crossfit(aa = aa, ww = ww, xx = xx, yy = yy, K = 2,
                         gamma = exp(seq(-4, 10, length.out = 20)),
                         DML = "DML1",
                         do_DML = TRUE,
                         do_regDML = TRUE,
                         cond_func_all = get_condexp_funcs(rep("ols", 3)),
                         params = params)
    check_beta_output_numeric(res_old, res)
  }

  aa <- aa_matone
  ww <- ww_frameone
  xx <- xx_matone
  yy <- yy_mat
  do_computations_numeric(aa, ww, xx, yy)

  aa <- aa_mat
  ww <- ww_frameone
  xx <- xx_matone
  yy <- yy_mat
  do_computations_numeric(aa, ww, xx, yy)

  aa <- aa_matone
  ww <- ww_frame
  xx <- xx_matone
  yy <- yy_mat
  do_computations_numeric(aa, ww, xx, yy)

  aa <- aa_mat
  ww <- ww_frame
  xx <- xx_matone
  yy <- yy_mat
  do_computations_numeric(aa, ww, xx, yy)
})

test_that("numerical estimators of beta and sigma are correct if d > 1", {
  check_beta_output_numeric <- function(res_old, res) {
    DML_old <- res_old$DML_statistics
    DML <- res$DML_statistics

    expect_equal(DML_old$beta_DML, DML$beta_DML, tolerance = tol)
    expect_equal(DML_old$sd_DML, DML$sd_DML, tolerance = tol)
    expect_equal(DML_old$var_DML, DML$var_DML, tolerance = tol)
    expect_equal(DML_old$pval_DML, DML$pval_DML, tolerance = tol)
    expect_equal(DML_old$CI_DML, DML$CI_DML, tolerance = tol)

    regsDML_old <- res_old$regsDML_statistics
    regsDML <- res$regsDML_statistics

    expect_equal(regsDML_old$beta_regsDML,
                 regsDML$beta_regsDML, tolerance = tol)
    expect_equal(regsDML_old$sd_regsDML,
                 regsDML$sd_regsDML, tolerance = tol)
    expect_equal(regsDML_old$var_regsDML,
                 regsDML$var_regsDML, tolerance = tol)
    expect_equal(regsDML_old$pval_regsDML,
                 regsDML$pval_regsDML, tolerance = tol)
    expect_equal(regsDML_old$CI_regsDML,
                 regsDML$CI_regsDML, tolerance = tol)
    expect_equal(regsDML_old$gamma_aN,
                 regsDML$gamma_aN, tolerance = tol)
    expect_equal(regsDML_old$message_regsDML,
                 regsDML$message_regsDML, tolerance = tol)
  }

  aa <- aa_mat
  ww <- ww_frame
  xx <- xx_mat
  yy <- yy_mat

  set.seed(5)
  load("res_old.RData")
  res <- suppressWarnings(regsdml(a = aa, w = ww, x = xx, y = yy,
                                  S = 1))
  check_beta_output_numeric(res_old, res)

  set.seed(5)
  res2 <- suppressWarnings(regsdml(a = aa, w = ww, x = xx, y = yy,
                                   S = 1, parallel = "no"))
  check_beta_output_numeric(res_old, res2)

  set.seed(5)
  res3 <- suppressWarnings(regsdml(a = aa, w = ww, x = xx, y = yy,
                                   S = 1, parallel = "multi"))
  check_beta_output_numeric(res_old, res3)

  set.seed(5)
  res4 <- suppressWarnings(regsdml(a = aa, w = ww, x = xx, y = yy,
                                   S = 1, parallel = "snow"))
  check_beta_output_numeric(res_old, res4)

  set.seed(5)
  ncpus <- 1
  cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
  res5 <- suppressWarnings(regsdml(a = aa, w = ww, x = xx, y = yy,
                                   S = 1, cl = cl))
  parallel::stopCluster(cl)
  check_beta_output_numeric(res_old, res5)
})
