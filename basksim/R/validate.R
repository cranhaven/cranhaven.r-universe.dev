val_borrow_cpp <- function(design, n, r, a, b,
                           globalweight_fun = NULL,
                           globalweight_params = list()) {
  cpp_fun <- function(x, n, a, b) {
    1 / (1 + exp(a + b * log(n^(1 / 4) * abs(x[1]/n - x[2]/n))))
  }

  shape1 <- shape2 <- numeric(length(r))
  if (!is.null(globalweight_fun)) {
    gw <- do.call(globalweight_fun, args = c(globalweight_params, n = n,
      r = list(r)))
  }

  for (i in 1:length(r)) {
    w <- sapply((1:length(r))[-i],
      function(x) cpp_fun(x = r[c(i, x)], n = n, a = a, b = b))

    if (!is.null(globalweight_fun)) {
      w <- w * gw
    }

    shape1[i] <- design$shape1 + r[i] + sum(r[-i] * w)
    shape2[i] <- design$shape2 + (n - r[i]) + sum((n - r[-i]) * w)
  }
  rbind(shape1, shape2)
}

val_borrow_jsdglobal <- function(design, n, r, epsilon, tau, logbase,
                              eps_all) {
  kl_fun <- function(x, y) {
    f <- function(z) x(z) * log(x(z) / y(z), base = logbase)
    stats::integrate(f, lower = 0, upper = 1)$value
  }

  jsd_fun <- function(sp1, sp2, n, epsilon, tau, logbase) {
    j1 <- function(x) stats::dbeta(x, shape1 = sp1[1], shape2 = sp2[1])
    j2 <- function(x) stats::dbeta(x, shape1 = sp1[2], shape2 = sp2[2])
    m <- function(x) (1 / 2) * (j1(x) + j2(x))
    jsd <- (1 / 2) * kl_fun(j1, m) + (1 / 2) * kl_fun(j2, m)
    w <- (1 - jsd)^epsilon
    ifelse(w <= tau, 0, w)
  }

  shape1 <- shape2 <- numeric(length(r))
  shape_prior1 <- design$shape1 + r
  shape_prior2 <- design$shape2 + (n - r)
  for (i in 1:length(r)) {
    w <- sapply((1:length(r))[-i],
      function(x) jsd_fun(sp1 = shape_prior1[c(i, x)],
        sp2 = shape_prior2[c(i, x)], n = n, epsilon = epsilon, tau = tau,
        logbase = logbase))
    w <- w * jsd_global4(rbind(shape_prior1, shape_prior2), epsilon = eps_all)
    shape1[i] <- shape_prior1[i] + sum(r[-i] * w)
    shape2[i] <- shape_prior2[i] + sum((n - r[-i]) * w)
  }
  rbind(shape1, shape2)
}

val_borrow_fujikawa <- function(design, n, r, epsilon, tau, logbase) {
  kl_fun <- function(x, y) {
    f <- function(z) x(z) * log(x(z) / y(z), base = logbase)
    stats::integrate(f, lower = 0, upper = 1)$value
  }

  jsd_fun <- function(sp1, sp2, n, epsilon, tau, logbase) {
    j1 <- function(x) stats::dbeta(x, shape1 = sp1[1], shape2 = sp2[1])
    j2 <- function(x) stats::dbeta(x, shape1 = sp1[2], shape2 = sp2[2])
    m <- function(x) (1 / 2) * (j1(x) + j2(x))
    jsd <- (1 / 2) * kl_fun(j1, m) + (1 / 2) * kl_fun(j2, m)
    w <- (1 - jsd)^epsilon
    ifelse(w <= tau, 0, w)
  }

  shape1 <- shape2 <- numeric(length(r))
  shape_prior1 <- design$shape1 + r
  shape_prior2 <- design$shape2 + (n - r)
  for (i in 1:length(r)) {
    w <- sapply((1:length(r))[-i],
      function(x) jsd_fun(sp1 = shape_prior1[c(i, x)],
        sp2 = shape_prior2[c(i, x)], n = n, epsilon = epsilon, tau = tau,
        logbase = logbase))
    shape1[i] <- shape_prior1[i] + sum(shape_prior1[-i] * w)
    shape2[i] <- shape_prior2[i] + sum(shape_prior2[-i] * w)
  }
  rbind(shape1, shape2)
}

jsd_global4 <- function(shape, epsilon) {
  f1 <- function(x) stats::dbeta(x, shape1 = shape[1, 1], shape2 = shape[2, 1])
  f2 <- function(x) stats::dbeta(x, shape1 = shape[1, 2], shape2 = shape[2, 2])
  f3 <- function(x) stats::dbeta(x, shape1 = shape[1, 3], shape2 = shape[2, 3])
  f4 <- function(x) stats::dbeta(x, shape1 = shape[1, 4], shape2 = shape[2, 4])
  m <- function(x) (f1(x) + f2(x) + f3(x) + f4(x)) / 4

  kl_fun <- function(x, y) {
    f <- function(z) x(z) * log(x(z) / y(z), base = 4)
    stats::integrate(f, lower = 0, upper = 1)$value
  }

  j1 <- kl_fun(f1, m)
  j2 <- kl_fun(f2, m)
  j3 <- kl_fun(f3, m)
  j4 <- kl_fun(f4, m)

  (1 - (j1 + j2 + j3 + j4) / 4)^epsilon
}

