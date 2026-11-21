library(CIMEHR)

# Pairwise Newton polishing with 20,000 subjects.
simulate <- function(n, seed, sd_logU, sd_q) {
  set.seed(seed)
  Z <- rep(c(0, 1), length.out = n)
  X <- rnorm(n)
  U <- exp(rnorm(n, -.5 * sd_logU^2, sd_logU))
  q <- rnorm(n, 0, sd_q)
  b <- .35 * log(U) + .4 * q + rnorm(n, 0, .6)
  m <- rpois(n, 10 * exp(.25 * Z - .15 * X) * U)
  tt <- e_r <- e_y <- vector('list', n)
  for (i in seq_len(n)) if (m[i]) {
    tt[[i]] <- sort(runif(m[i], .025, 4.975))
    e_r[[i]] <- rnorm(m[i])
    e_y[[i]] <- rnorm(m[i], 0, .7)
  }
  id <- rep(seq_len(n), pmax(m, 1))
  visit <- m[id] > 0
  time <- Y <- rep(NA_real_, length(id))
  R <- rep(NA_integer_, length(id))
  time[visit] <- unlist(tt)
  j <- id[visit]
  R[visit] <- as.integer(unlist(e_r) < .25 + .2 * Z[j] - .1 * X[j] +
                           .55 * log(U[j]) + q[j])
  Y[visit] <- 1 + .8 * Z[j] - .4 * X[j] + .2 * time[visit] + b[j] + unlist(e_y)
  Y[visit][R[visit] == 0L] <- NA_real_
  data.frame(id = id, time = time, Y = Y, R = R, Z = Z[id], X = X[id], C = 5)
}

dat <- simulate(20000, 300301, .7, .15)
fit <- fit_method('cimehr', dat, visit_covs = c('Z', 'X'),
                  long_covs = c('Z', 'X'), censor_col = 'C', seed = 1)
pairwise <- fit$fit$observation_process$pairwise_diagnostics
stopifnot(pairwise$score_tolerance == 1e-10,
          pairwise$scaled_kkt_error <= 1e-10)
