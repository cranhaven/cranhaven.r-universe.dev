avar <- function(x, sigma, type, parallel) {
  if (parallel) {
    if (type == "normal" || type == "elliptical") {
      avar_parallel_elliptical(alpha(sigma), x, type)
    } else {
      avar_parallel_adf(alpha(sigma), sigma, gamma_mat(x, type = "adf"))
    }
  } else {
    if (type == "normal" || type == "elliptical") {
      avar_elliptical(x, sigma, type)
    } else {
      avar_adf(x, sigma)
    }
  }
}

avar_std <- function(x, sigma, type, parallel) {
  if (parallel) {
    if (type == "normal" || type == "elliptical") {
      avar_parallel_elliptical(alpha_std(sigma), x, type)
    } else {
      avar_parallel_adf(alpha_std(sigma), sigma, gamma_mat(x, type = "adf"))
    }
  } else {
    avar_std_adf(x, sigma, type)
  }
}

avar_adf <- function(x, sigma) {
  k <- ncol(sigma)
  gamma_mat_ <- gamma_mat(x)
  d_mat <- matrixcalc::D.matrix(k)
  vec <- t(d_mat) %*% (tr(sigma) * rep(1, k^2) - sum(sigma) * c(diag(k)))
  c(t(vec) %*% gamma_mat_ %*% vec) / sum(sigma)^4 * (k / (k - 1))^2
}

avar_elliptical <- function(x, sigma, type) {
  corr <- kurtosis_correction(x, type)
  k <- ncol(sigma)
  sigma2 <- sigma %*% sigma
  sum_s <- sum(sigma)
  tr_s <- tr(sigma)
  sum_s2 <- sum(sigma2)
  tr_s2 <- tr(sigma2)
  q_diff <- sum_s * (tr_s2 + tr_s^2) - 2 * tr_s * sum_s2
  q_mult <- 2 * corr * (k^2 / (k - 1)^2) / sum_s^3
  q_diff * q_mult
}

avar_parallel_elliptical <- function(alpha, x, type) {
  k <- ncol(x)
  2 * k / (k - 1) * (1 - alpha)^2 * kurtosis_correction(x, type)
}

avar_parallel_adf <- function(alpha, sigma, gamma) {
  k <- ncol(sigma)
  corr <- stats::cov2cor(sigma)
  rho <- (sum(corr) - k) / (k * (k - 1))
  phi <- sqrt(mean(diag(sigma)))
  nu <- ((k - 1) * rho + 1) * c(diag(k)) - rep(1, k^2)
  if (nrow(gamma) < k^2) {
    d_mat <- matrixcalc::D.matrix(k)
    gamma <- d_mat %*% gamma %*% t(d_mat)
  }
  c(alpha^4 / (k - 1)^2 * (k * rho * phi)^(-4) * t(nu) %*% gamma %*% nu)
}

avar_std_adf <- function(x, sigma, type) {
  k <- ncol(sigma)
  phi <- stats::cov2cor(sigma)
  psi_mat_ <- psi_mat(x, sigma, type)
  gs_ <- gs(phi)
  c(t(gs_) %*% psi_mat_ %*% gs_) / sum(phi)^4 * (k / (k - 1))^2
}

#' Gamma matrix
#'
#' Calculate the gamma matrix from a matrix of observations.
#' @param x A numeric matrix of observations.
#' @param sigma Covariance matrix of the data.
#' @param type One of `adf`, `normal` and `elliptical`.
#' @return The sample estimate of the gamma matrix.
#' @keywords internal
gamma_mat <- function(x, sigma, type = "adf") {
  if (type == "adf") {
    i_row <- \(n) unlist(lapply(seq_len(n), seq.int, n))
    i_col <- \(n) rep.int(seq_len(n), times = rev(seq_len(n)))
    y <- t(x) - colMeans(x, na.rm = TRUE)
    z <- y[i_col(ncol(x)), , drop = FALSE] * y[i_row(ncol(x)), , drop = FALSE]
    base::tcrossprod(z - rowMeans(z, na.rm = TRUE)) / nrow(x)
  } else {
    gamma_mat_normal(sigma) * kurtosis_correction(x, type = type)
  }
}

gamma_mat_normal <- function(sigma) {
  k <- ncol(sigma)
  k_mat <- matrixcalc::K.matrix(k)
  ((diag(k^2) + k_mat) %*% (sigma %x% sigma))
}

#' Psi matrix
#'
#' Calculate the psi matrix from a matrix of observations.
#' @param x A numeric matrix of observations.
#' @param sigma Covariance matrix.
#' @param type One of `adf`, `normal` and `elliptical`.
#' @return The sample estimate of the psi matrix.
#' @keywords internal
psi_mat <- function(x, sigma, type = "adf") {
  d_mat <- matrixcalc::D.matrix(ncol(sigma))
  sigma_d <- diag(1 / sqrt(diag(sigma)))
  sdxsd <- (sigma_d %x% sigma_d)
  if (type == "adf") {
    multiplier <- sdxsd %*% d_mat
    mat <- gamma_mat(x)
  } else {
    multiplier <- sdxsd
    mat <- gamma_mat(x, sigma, type)
  }
  multiplier %*% mat %*% t(multiplier)
}

#' The gs vector used in the asymptotic variance of standardized alpha.
#'
#' @param phi Correlation matrix.
#' @return The gs vector.
#' @keywords internal
gs <- function(phi) {
  k <- ncol(phi)
  k_mat <- matrixcalc::K.matrix(k)
  k_mat_d <- diag(diag(k_mat))
  k - k * k_mat_d %*% c(phi %*% matrix(1, k, k))
}

#' Calculate unbiased sample kurtosis.
#' @param x Matrix of valus.
#' @return Unbiased sample kurtosis.
#' @keywords internal
kurtosis <- function(x) {
  n <- nrow(x)
  g2 <- \(x) mean((x - mean(x))^4) / stats::var(x)^2
  kurtosis <- \(x) (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * g2(x) + 6)
  mean(apply(x, 2, kurtosis)) - 3
}

#' Calculate kurtosis correction
#' @param x Matrix of values
#' @param type The type of correction, either "normal" or "elliptical".
#' @keywords internal
kurtosis_correction <- function(x, type) {
  kurt <- if (type == "normal") 0 else kurtosis(x)
  1 + kurt / 3
}
