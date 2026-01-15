LaplaceBeltrami <- function(qspray, alpha) {
  n <- numberOfVariables(qspray)
  derivatives1 <- lapply(seq_len(n), function(i) {
    derivQspray(qspray, i)
  })
  derivatives2 <- lapply(seq_len(n), function(i) {
    derivQspray(derivatives1[[i]], i)
  })
  x <- lapply(seq_len(n), qlone) # x_1, x_2, ..., x_n
  # first term
  out1 <- 0L
  for(i in seq_len(n)) {
    out1 <- out1 + alpha * x[[i]]^2 * derivatives2[[i]]
  }
  # second term
  out2 <- 0L
  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      if(i != j) {
        out2 <- out2 + x[[i]]^2 * derivatives1[[i]] / (x[[i]] - x[[j]])
      }
    }
  }
  # at this step, `out2` is a `ratioOfQsprays` object, because of the divisions
  # by `x[[i]] - x[[j]]`; but actually its denominator is 1 because of some
  # simplifications and then we extract its numerator to get a `qspray` object
  out2 <- getNumerator(out2)
  out1/2 + out2
}

eigenvalueLB <- function(n, lambda, alpha) {
  b <- function(mu) {
    c(crossprod(mu, seq_along(mu)-1L))
  }
  alpha * b(dualPartition(lambda)) - b(lambda) + (n - 1) * sum(lambda)
}

CalogeroSutherland <- function(qspray, alpha) {
  n <- numberOfVariables(qspray)
  dx <- lapply(seq_len(n), function(i) {
    derivQspray(qspray, i)
  })
  x <- lapply(seq_len(n), qlone) # x_1, x_2, ..., x_n
  # first term
  op0 <- function(p, i) {
    x[[i]] * derivQspray(p, i)
  }
  op1 <- function(i) {
    op0(op0(qspray, i), i)
  }
  toSum <- lapply(1L:n, op1)
  out1 <- Reduce(`+`, toSum)
  # second term
  out2 <- 0L
  for(j in 2L:n) {
    for(i in 1L:(j-1L)) {
      out2 <- out2 +
        (x[[i]] + x[[j]]) * (x[[i]] * dx[[i]] - x[[j]] * dx[[j]]) /
          (x[[i]] - x[[j]])
    }
  }
  # at this step, `out2` is a `ratioOfQsprays` object, because of the divisions
  # by `x[[i]] - x[[j]]`; but actually its denominator is 1 because of some
  # simplifications and then we extract its numerator to get a `qspray` object
  out2 <- getNumerator(out2)
  #
  (alpha * out1 + out2) / 2
}

eigenvalueCS <- function(n, lambda, alpha) {
  toSum <- lapply(seq_along(lambda), function(i) {
    alpha*lambda[i]^2 + (n + 1 - 2*i)*lambda[i]
  })
  sum(gmp::c_bigq(toSum)) / 2
}
