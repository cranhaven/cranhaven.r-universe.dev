#' @importFrom stats setNames

### time_difference -----------------------------------------------------------
time_difference <- function(n, t_in) {
  delta_t_in <- numeric(n - 1)

  for (i in 1:(n - 1)) {
    delta_t_in[i] <- t_in[i + 1] - t_in[i]
    if (delta_t_in[i] < 0) {
      stop("non-increasing measurement times")
    }
  }

  delta_t_in
}

### extend_time_diff ----------------------------------------------------------
extend_time_diff <- function(n, t_dat) {
  delta_t_dat <- numeric(n)

  for (i in 1:(n - 1)) {
    delta_t_dat[i] <- t_dat[i + 1] - t_dat[i]
  }

  delta_t_dat[n] <- 0

  delta_t_dat <- matrix(delta_t_dat, ncol = 1)

  delta_t_dat
}

### create_objective_fun ------------------------------------------------------
create_objective_fun <- function(J, W, M, mu, y_dat) {
  function(x) {
    t(W %*% ((1 / mu) * (pracma::mldivide(J, (M %*% x), pinv = TRUE)) - y_dat)) %*%
      (W %*% ((1 / mu) * (pracma::mldivide(J, (M %*% x), pinv = TRUE)) - y_dat))
  }
}

### j_matrix ------------------------------------------------------------------
j_matrix <- function(mu, n, delta_t_dat) {
  J <- matrix(0, nrow = n, ncol = n)

  for (i in 1:(n - 1)) {
    J[i, i] <- 1
    J[i + 1, i] <- -exp(-mu * delta_t_dat[i])
  }

  J[n, n] <- 1
  J[1, n] <- -exp(-mu * delta_t_dat[n])

  J
}

### m_matrix ------------------------------------------------------------------
m_matrix <- function(mu, n, delta_t_dat) {
  M <- matrix(0, nrow = n, ncol = n)

  for (i in 2:(n - 1)) {
    M[i, i] <- (1 - exp(-mu * delta_t_dat[i - 1])) / (mu * delta_t_dat[i - 1]) - exp(-mu * delta_t_dat[i - 1])
    M[i + 1, i] <- 1 - (1 - exp(-mu * delta_t_dat[i])) / (mu * delta_t_dat[i])
  }

  M[n, n] <- (1 - exp((-1) * mu * delta_t_dat[n - 1])) / (mu * delta_t_dat[(n - 1)]) - exp(-mu * delta_t_dat[(n - 1)])
  M[1, n] <- 0
  M[1, 1] <- 0
  M[2, 1] <- 1 - (1 - exp(-mu * delta_t_dat[1])) / (mu * delta_t_dat[(1)])

  M
}

### weight_matrix -------------------------------------------------------------
weight_matrix <- function(mu, m, n, t_in, n_pts) {
  W <- matrix(0, nrow = n, ncol = n)

  if (t_in[(1)] - m / mu > 0) {
    W[1, 1] <- 1
    W[2, 2] <- 1
    W[n, n] <- 1

    for (i in 3:n - 1) {
      W[i, i] <- sqrt(1)
    }
  } else {
    W[1, 1] <- 1
    W[n - 1, n_pts - 1] <- 1
    W[n, n] <- 1

    for (i in 2:n - 2) {
      W[i, i] <- sqrt(1)
    }
  }

  W
}

### aeq -----------------------------------------------------------------------
aeq <- function(n) {
  aeq <- matrix(numeric(n), nrow = 1)
  aeq[(1)] <- 1
  aeq[(n)] <- -1

  aeq
}

### lam_fourier ---------------------------------------------------------------
cal_lam_fourier <- function(n_lam, n, tau, lambda_dat, t_dat, delta_t_dat) {
  lam_fourier <- numeric(n_lam + 1)

  for (j in 1:(n - 1)) {
    lam_fourier[1] <- lam_fourier[1] + (1 / tau) * (lambda_dat$par[j] + lambda_dat$par[j + 1]) * delta_t_dat[j] / 2
  } # zero mode

  # non-zero postive modes (negative modes given by complex conjugates of positve modes)
  for (k in 2:(n_lam + 1)) {
    for (j in 1:(n - 1)) {
      lam_fourier[(k)] <- lam_fourier[(k)] +
        ((lambda_dat$par[(j + 1)] - lambda_dat$par[(j)]) / delta_t_dat[(j)]) *
        (tau / (2 * pi * (k - 1))) *
        ((exp(-2 * pi * complex(real = 0, imaginary = 1) *
                (k - 1) * t_dat[(j + 1)] / tau) -
            exp(-2 * pi * complex(real = 0, imaginary = 1) *
                  (k - 1) * t_dat[(j)] / tau)) /
           (2 * pi * (k - 1))) + complex(real = 0, imaginary = 1) *
        (lambda_dat$par[(j + 1)] *
           exp(-2 * pi * complex(real = 0, imaginary = 1) * (k - 1) *
                 t_dat[(j + 1)] / tau) -
           lambda_dat$par[(j)] *
           exp(-2 * pi * complex(real = 0, imaginary = 1) * (k - 1) *
                 t_dat[(j)] / tau)) /
        (2 * pi * (k - 1))
    }
  }

  lam_fourier
}
