# Weight matrix with JSD weights
get_weights_jsd <- function(design, n, epsilon, tau, logbase, ...) {
  shape1_post <- design$shape1 + c(0:n)
  shape2_post <- design$shape2 + c(n:0)
  n_sum <- n + 1

  p <- function(x) stats::dbeta(x, shape1_post[i], shape2_post[i])
  q <- function(x) stats::dbeta(x, shape1_post[j], shape2_post[j])
  m <- function(x) 0.5 * (p(x) + q(x))
  f <- function(x) p(x) * log(p(x) / m(x), base = logbase)
  g <- function(x) q(x) * log(q(x) / m(x), base = logbase)
  h <- function(x) 0.5 * f(x) + 0.5 * g(x)
  jsd_mat <- matrix(0, nrow = n_sum, ncol = n_sum)
  for (i in 1:n_sum) {
    for (j in i:n_sum) {
      if (i == j) {
        next
      } else {
        kl_f <- stats::integrate(f, 0, 1)$value
        kl_g <- stats::integrate(g, 0, 1)$value
        jsd_mat[i, j] <- 0.5 * kl_f + 0.5 * kl_g
      }
    }
  }
  jsd_mat <- jsd_mat + t(jsd_mat)
  weight_mat <- (1 - jsd_mat)^epsilon
  weight_mat[weight_mat <= tau] <- 0

  weight_mat
}

# Weight matrix with MML weights
get_weights_mml <- function(design, n, ...) {
  n_sum <- n + 1
  mat <- matrix(0, nrow = n_sum, ncol = n_sum)
  r <- 0:n
  for (i in 1:n_sum) {
    for (j in 1:n_sum) {
      f <- function(delta) -extraDistr::dbbinom(
        x = r[i],
        size = n,
        alpha = design$shape1 + delta * r[j],
        beta = design$shape2 + delta * (n - r[j])
      )

      l <- stats::optim(0.5, fn = f, lower = 0, upper = 1,
        method = "Brent")$par
      mat[i, j] <- ifelse(l <= 6.474096e-09, 0, l)
    }
  }
  mat <- (mat + t(mat)) / 2
  diag(mat) <- 1
  mat
}

# Weight matrix with CPP weights
get_weights_cpp <- function(n, tune_a = 1, tune_b = 1, ...) {
  n_sum <- n + 1
  weight_mat <- matrix(0, nrow = n_sum, ncol = n_sum)
  r1 <- r2 <- 0:n

  g <- function(s, a, b) {
    1 / (1 + exp(a + b * log(s)))
  }

  for (i in 1:n_sum) {
    for (j in i:n_sum) {
      if (i == j) {
        next
      } else {
        vec1 <- rep(0:1, c(n - r1[i], r1[i]))
        vec2 <- rep(0:1, c(n - r2[j], r2[j]))
        ks <- suppressWarnings(stats::ks.test(vec1, vec2)$statistic)
        s <- n^(1/4) * ks
        weight_mat[i, j] <- g(s = s, a = tune_a, b = tune_b)
      }
    }
  }
  weight_mat <- weight_mat + t(weight_mat)
  diag(weight_mat) <- 1
  weight_mat
}
