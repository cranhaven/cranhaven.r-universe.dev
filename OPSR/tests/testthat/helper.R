load_sim_dat <- function() {
  sim_dat <- readRDS(test_path("fixtures", "sim_dat.rds"))
  sim_dat
}

sim4 <- function(nobs = 1000, sigma = NULL) {
  ## same regressors in selection and outcome might lead to identification issues
  X <- obs_mat(nobs, p = 2, sd = 1)
  colnames(X) <- paste0("xo", 1:ncol(X))
  W <- obs_mat(nobs, p = 2, sd = 1)
  colnames(W) <- paste0("xs", 1:ncol(X))

  if (is.null(sigma)) {
    sigma <- matrix(c(
      1.0, 0.2, 0.3, 0.4, 0.5,
      0.2, 1.1, 0.4, 0.5, 0.6,
      0.3, 0.4, 1.2, 0.6, 0.7,
      0.4, 0.5, 0.6, 1.3, 0.2,
      0.5, 0.6, 0.7, 0.2, 1.0), ncol = 5)  # J = 4
  }
  err <- errors(sigma, nobs)

  e <- err[, 1]
  eta1 <- err[, 2]
  eta2 <- err[, 3]
  eta3 <- err[, 4]
  eta4 <- err[, 5]
  gamma <- c(1, 1.5)
  cali_z <- cali_Z(gamma, W, e)
  kappa <- c(-2, 1, 1.5)
  z <- Z(kappa, J = 4, cali_z)

  b1 <- c(1, 2, 1.1)
  b2 <- c(1, -1, 1.5)
  b3 <- c(1, 1.6, -2)
  b4 <- c(1, -1.5, 2)
  X_ <- cbind(1, X)
  y1 <- y_j(X_, b1, eta1)
  y2 <- y_j(X_, b2, eta2)
  y3 <- y_j(X_, b3, eta3)
  y4 <- y_j(X_, b4, eta4)

  params <- list(
    gamma = gamma,
    kappa = kappa,
    beta1 = b1,
    beta2 = b2,
    beta3 = b3,
    beta4 = b4
  )

  data <- data.frame(
    ys = z,
    yo = ifelse(z == 1, y1, ifelse(z == 2, y2, ifelse(z == 3, y3, y4))),
    W,
    X
  )

  out <- list(
    params = params,
    data = data,
    errors = err,
    sigma = sigma
  )

  out
}
