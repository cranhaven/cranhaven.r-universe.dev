###########################################
# Data generating mechanism for the example
###########################################

datafun_ranef <- function(N, n) {
  # balanced groups of about the same size
  nT <- n + sample(-3:3, N, replace = TRUE)
  id <- do.call(c, sapply(seq_len(N), function(x) rep(x, nT[x])))
  list(zz = data.frame(id = id), nT = nT)
}

gW_Y <- function(ww) {
  (ww[, 1] >= 0) * (ww[, 3] >= 0)
}

gW_X <- function(ww) {
  (ww[, 1] <= 0) * (ww[, 2] >= 0)
}

Sigma0 <- function() {
  list(sigma0 = 0.1,
       theta_factor0 = 0.05)
}

return_values <- function(xx, yy, ww, zz, nT, beta) {
  N <- length(nT)
  cask_factor <-
    do.call(c, sapply(seq_len(N), function(x)
      c(rep(1, floor(nT[x] / 2)), rep(2, ceiling(nT[x] / 2)))))
  ww_data_frame <- as.data.frame(ww)
  colnames(ww_data_frame) <-
    sapply(seq_len(ncol(ww_data_frame)), function(a) paste("w", a, sep = ""))
  data_interm <-
    data.frame(x1 = xx[, 1],
               resp = yy, data.frame(id = as.factor(zz$id),
                                     cask = as.factor(cask_factor)),
               ww_data_frame)
  if (length(beta) == 1) {
    data_interm
  } else if (length(beta) == 2) {
    cbind(data_interm, data.frame(x2 = xx[, 2]))
  }
}

example_data_mmdml <- function(beta0, N = 10L, n = 5L) {
  if (n <= 4) {
    stop("Choose n >= 5.")
  }
  if (length(beta0) > 2) {
    stop("Choose beta0 of length 1 or 2.")
  }

  if (length(beta0) == 1) {
    beta0 <- c(beta0, 0)
  }
  beta0 <- matrix(beta0, ncol = 1)

  res_ranef <- datafun_ranef(N, n)
  nT <- res_ranef$nT
  Ntot <- sum(nT)
  zz = res_ranef$zz

  sigma_theta <- Sigma0()
  sigma0 <- sigma_theta$sigma0
  theta0 <- sigma_theta$theta_factor0 * c(1, 1) / sigma0

  ranef <- rnorm(N, 0, theta0[1] * sigma0)
  cask1 <- rnorm(N, 0, theta0[2] * sigma0)
  cask2 <- rnorm(N, 0, theta0[2] * sigma0)
  n <- sum(nT)
  b <- do.call(c, sapply(seq_len(N), function(x) rep(ranef[x], nT[x])))
  cask <-
    do.call(c, sapply(seq_len(N), function(x)
      c(rep(cask1[x], floor(nT[x] / 2)), rep(cask2[x], ceiling(nT[x] / 2)))))

  ww <- do.call(cbind, lapply(seq_len(3), function(x) rnorm(n, 0, 1)))
  xx <- cbind(gW_X(ww) + rnorm(n, 0, 0.5), rnorm(n, 0, 1))
  yy <- xx %*% beta0 + gW_Y(ww) + cask + b  + rnorm(n, 0, sigma0)

  return_values(xx = xx, yy = yy, ww = ww, zz = zz, nT = nT, beta = beta0[beta0 != 0])
}
