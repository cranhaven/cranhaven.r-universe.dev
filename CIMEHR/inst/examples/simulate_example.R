# Reproduce the simulated data supplied with CIMEHR.
# Usage: Rscript simulate_example.R path/to/cimehr_example.rda

simulate_cimehr_example <- function(n = 1000L, seed = 200000013L) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 2L, n == as.integer(n))
  n <- as.integer(n)
  RNGkind('Mersenne-Twister', 'Inversion', 'Rejection')
  set.seed(seed)
  Z <- stats::rbinom(n, 1, 0.5)
  X <- stats::rnorm(n, 0, 1)
  U <- stats::rnorm(n, 0, 1)
  sigma <- 0.6
  eta <- exp(sigma * U - sigma^2/2)
  gamma <- c(intercept = -1.6, Z = 0.4, X = 0.2)
  rate <- eta * exp(gamma[1] + gamma[2] * Z + gamma[3] * X)
  b_innovation <- matrix(stats::rnorm(n * 2L), nrow = n)
  q_innovation <- matrix(stats::rnorm(n * 2L), nrow = n) *
    matrix(rep(sqrt(c(0.2, 0.2)), each = n), nrow = n)
  q <- cbind(0.2 * U, 0.8 * U) + q_innovation

  rows <- vector('list', n)
  for (i in seq_len(n)) {
    count <- stats::rpois(1L, rate[i] * 60)
    time <- if (count) sort(stats::runif(count, 0, 60)) else numeric()
    if (!length(time)) {
      rows[[i]] <- data.frame(id = i, time = NA_real_, Y = NA_real_, R = NA_integer_)
      next
    }
    s <- time/60
    mu <- -2 + 6 * s + 0 * s^2 - 0.5 * Z[i] + 0.5 * X[i] +
      (b_innovation[i, 1] + b_innovation[i, 2] * Z[i] + 0 * Z[i] * time/60)
    y <- mu + stats::rnorm(length(time), sd = 1)
    observation_predictor <- 0.2 - 0.4 * Z[i] + 0 * X[i] + q[i, 1] + q[i, 2] * Z[i]
    recorded <- stats::rbinom(length(time), 1L, stats::pnorm(observation_predictor))
    rows[[i]] <- data.frame(id = i, time = time,
      Y = ifelse(recorded == 1L, y, NA_real_), R = recorded)
  }
  generated <- do.call(rbind, rows)
  conditional_Sigma_b <- matrix(c(0.75, -0.1, -0.1, 1.96), 2L, 2L,
    dimnames = list(c('b0', 'bZ'), c('b0', 'bZ')))
  theta <- c(b0 = 0.5, bZ = 0.2)
  b <- outer(U, unname(theta)) + b_innovation %*% chol(conditional_Sigma_b)
  id <- generated$id
  generated$Y <- generated$Y + (b[id, 1] - b_innovation[id, 1]) +
    (b[id, 2] - b_innovation[id, 2]) * Z[id]
  visits <- generated[is.finite(generated$time), c('id', 'time', 'Y', 'R')]
  visits$id <- as.integer(visits$id)
  visits$R <- as.integer(visits$R)
  rownames(visits) <- NULL
  participants <- data.frame(id = seq_len(n), Z = Z, X = X, C = 60)
  truth <- list(seed = as.integer(seed), n = n, horizon = 60,
    sigma = sigma, sigma2_zeta = sigma^2, gamma = gamma,
    alpha = c(intercept = 0.2, Z = -0.4, X = 0),
    delta = c(q0 = 0.2, qZ = 0.8), theta = theta,
    beta = c(Z = -0.5, X = 0.5),
    conditional_Sigma_q = matrix(c(0.2, 0, 0, 0.2), 2L, 2L,
      dimnames = list(c('q0', 'qZ'), c('q0', 'qZ'))),
    conditional_Sigma_b = conditional_Sigma_b,
    marginal_Sigma_b = conditional_Sigma_b + tcrossprod(unname(theta)),
    error_sd = 1, baseline_intercept = -2, baseline_slope = 0.1)
  list(participants = participants, visits = visits, truth = truth)
}

cimehr_example <- simulate_cimehr_example()

if (sys.nframe() == 0L) {
  output <- commandArgs(trailingOnly = TRUE)
  if (length(output) != 1L) {
    stop('Supply one output filename: Rscript simulate_example.R path/to/cimehr_example.rda')
  }
  save(cimehr_example, file = output, compress = 'xz', version = 2L)
}
