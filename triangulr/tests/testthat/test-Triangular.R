library(testthat)
library(triangulr)

################################################################################
## Setup

dtri_test <- function(x, a, b, c) {
  p <- 2 * (x - a) / ((b - a) * (c - a))
  p[x == c] <- 2 / (b - a)
  m <- c < x & x <= b
  p[m] <- 2 * (b - x[m]) / ((b - a) * (b - c))
  p[x < a | b < x] <- 0
  p
}

dtri_vec_test <- function(x, a, b, c) {
  p <- 2 * (x - a) / ((b - a) * (c - a))
  i <- x == c
  p[i] <- 2 / (b - a[i])
  i <- c < x & x <= b
  p[i] <- 2 * (b - x[i]) / ((b - a[i]) * (b - c))
  p[x < a | b < x] <- 0
  p
}

ptri_test <- function(q, a, b, c) {
  p <- (q - a) ^ 2 / ((b - a) * (c - a))
  p[q <= a] <- 0
  p[b <= q] <- 1
  i <- c < q & q < b
  p[i] <- 1 - (b - q[i]) ^ 2 / ((b - a) * (b - c))
  p
}

ptri_vec_test <- function(q, a, b, c) {
  p <- (q - a) ^ 2 / ((b - a) * (c - a))
  p[q <= a] <- 0
  p[b <= q] <- 1
  i <- c < q & q < b
  p[i] <- 1 - (b - q[i]) ^ 2 / ((b - a[i]) * (b - c))
  p
}

qtri_test <- function(p, a, b, c) {
  q <- a + sqrt(p * (b - a) * (c - a))
  i <- p >= (c - a) / (b - a)
  q[i] <- b - sqrt((1 - p[i]) * (b - a) * (b - c))
  q
}

qtri_vec_test <- function(p, a, b, c) {
  q <- a + sqrt(p * (b - a) * (c - a))
  i <- p >= (c - a) / (b - a)
  q[i] <- b - sqrt((1 - p[i]) * (b - a[i]) * (b - c))
  q
}

before_each <- function() {
  set.seed(1)
}

rtri_rand <- function(n) {
  before_each()
  runif(n)
}

rtri_test <- function(n, a, b, c) {
  p <- rtri_rand(n)
  q <- a + sqrt(p * (b - a) * (c - a))
  i <- p >= (c - a) / (b - a)
  q[i] <- b - sqrt((1 - p[i]) * (b - a) * (b - c))
  q
}

rtri_vec_test <- function(n, a, b, c) {
  p <- rtri_rand(n)
  q <- a + sqrt(p * (b - a) * (c - a))
  i <- p >= (c - a) / (b - a)
  q[i] <- b - sqrt((1 - p[i]) * (b - a[i]) * (b - c))
  q
}

mgtri_test <- function(t, a, b, c) {
  numer <- (b - c) * exp(a * t) -
    (b - a) * exp(c * t) +
    (c - a) * exp(b * t)
  denom <- (b - a) * (c - a) * (b - c) * t ^ 2
  2 * numer / denom
}

estri_test <- function(p, min, max, mode) {
  es <- function(x, a, b, c) {
    integrate(
      qtri,
      lower = 0,
      upper = x,
      min = a,
      max = b,
      mode = c
    )$value / x
  }
  mapply(es,
         x = p,
         a = min,
         b = max,
         c = mode)
}

expect_equal2 <- function(x, y, r = 5) {
  expect_equal(round(x, r), round(y, r))
}

###############################################################################
# Test cases for the probability density function

test_that("scalar x, scalar params, symmetric", {
  d <- dtri(0.5,
            min = 0,
            max = 1,
            mode = 0.5)
  d_test <- dtri_test(0.5, 0, 1, 0.5)
  expect_equal(d, d_test)
})

test_that("scalar x, scalar params, non-symmetric", {
  d <- dtri(0.5,
            min = 0,
            max = 1,
            mode = 0.8)
  d_test <- dtri_test(0.5, 0, 1, 0.8)
  expect_equal(d, d_test)
})

test_that("scalar x, scalar params, non-symmetric, log-scale", {
  d <- dtri(
    0.5,
    min = 0,
    max = 1,
    mode = 0.8,
    log = TRUE
  )
  d_test <- dtri_test(0.5, 0, 1, 0.8)
  expect_equal(exp(d), d_test)
})

test_that("vector x, scalar params, symmetric", {
  d <- dtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.5)
  d_test <- dtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.5)
  expect_equal(d, d_test)
})

test_that("vector x, scalar params, non-symmetric", {
  d <- dtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.8)
  d_test <- dtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(d, d_test)
})

test_that("vector x, scalar params, non-symmetric, log-scale", {
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = 0,
    max = 1,
    mode = 0.8,
    log = TRUE
  )
  d_test <- dtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(exp(d), d_test)
})

test_that("vector x, vector params, symmetric", {
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 2,
    mode = 1
  )
  d_test <- dtri_vec_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 2, 1)
  expect_equal(d, d_test)
})

test_that("vector x, vector params, non-symmetric", {
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = 7,
    mode = 6
  )
  d_test <- dtri_vec_test(c(0.1, 0.6, 0.9), c(0, 1, 2), 7, 6)
  expect_equal(d, d_test)
})

test_that("vector x, vector params, non-symmetric, log-scale", {
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = 7,
    mode = 6,
    log = TRUE
  )
  d_test <- dtri_vec_test(c(0.1, 0.6, 0.9), c(0, 1, 2), 7, 6)
  expect_equal(exp(d), d_test)
})

test_that("Mode at bound, min == mode", {
  d <- dtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0)
  d_test <- dtri_test(c(0.1, 0.6, 0.9), 0, 1, 0)
  expect_equal(d, d_test)
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = 3,
    mode = 2
  )
  d_test <- dtri_vec_test(c(0.1, 0.6, 0.9), c(0, 1, 2), 3, 2)
  expect_equal(d, d_test)
})

test_that("Mode at bound, max == mode", {
  d <- dtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 1)
  d_test <- dtri_test(c(0.1, 0.6, 0.9), 0, 1, 1)
  expect_equal(d, d_test)
  d <- dtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = 3,
    mode = 3
  )
  d_test <- dtri_vec_test(c(0.1, 0.6, 0.9), c(0, 1, 2), 3, 3)
  expect_equal(d, d_test)
})

test_that("Zeros produced, x < min", {
  d <- dtri(c(1, 2, 3),
            min = 2,
            max = 4,
            mode = 3)
  d_test <- dtri_test(c(1, 2, 3), 2, 4, 3)
  expect_equal(d, d_test)
  d <- dtri(c(1, 2, 3),
            min = c(2, 3, 4),
            max = 6,
            mode = 5)
  d_test <- dtri_vec_test(c(1, 2, 3), c(2, 3, 4), 6, 5)
  expect_equal(d, d_test)
})

test_that("Zeros produced, x > max", {
  d <- dtri(c(1, 2, 3),
            min = 0,
            max = 2,
            mode = 1)
  d_test <- dtri_test(c(1, 2, 3), 0, 2, 1)
  expect_equal(d, d_test)
  d <- dtri(c(2, 3, 4),
            min = c(0, 1, 2),
            max = 3,
            mode = 2)
  d_test <- dtri_vec_test(c(2, 3, 4), c(0, 1, 2), 3, 2)
  expect_equal(d, d_test)
})

test_that("NaN produced, mode < min", {
  d <- expect_warning(dtri(
    1,
    min = 0,
    max = 2,
    mode = -1
  ))
  expect_equal(d, NaN)
  d <- expect_warning(dtri(
    c(1, 2, 3),
    min = c(1, 2, 3),
    max = 4,
    mode = 2
  ))
  expect_equal(d, c(0, 1, NaN))
})

test_that("NaN produced, min == mode == max", {
  d <- expect_warning(dtri(
    1,
    min = 3,
    max = 3,
    mode = 3
  ))
  expect_equal(d, NaN)
  d <- expect_warning(dtri(
    c(0, 1, 2),
    min = c(0, 1, 2),
    max = 2,
    mode = 2
  ))
  expect_equal(d, c(0, 0, NaN))
})

test_that("NaN produced, min > max", {
  d <- expect_warning(dtri(
    1,
    min = 0,
    max = -1,
    mode = 1
  ))
  expect_equal(d, NaN)
  d <- expect_warning(dtri(
    c(1, 1.5, 2),
    min = c(1, 1, 3),
    max = 2,
    mode = 2
  ))
  expect_equal(d, c(0, 1, NaN))
})

test_that("Error, NULL arguments", {
  expect_error(dtri(x = NULL))
  expect_error(dtri(x = 1, min = NULL))
  expect_error(dtri(x = 1, max = NULL))
  expect_error(dtri(x = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(dtri(x = "1"))
  expect_error(dtri(x = 1, min = "0"))
  expect_error(dtri(x = 1, max = "1"))
  expect_error(dtri(x = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(dtri(x = 1, log = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(dtri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(dtri(
    seq(0.1, 1, 0.1),
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(dtri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})

################################################################################
## Test cases for the cumulative distribution function

test_that("scalar q, scalar params, symmetric", {
  p <- ptri(0.5,
            min = 0,
            max = 1,
            mode = 0.5)
  p_test <- ptri_test(0.5, 0, 1, 0.5)
  expect_equal(p, p_test)
})

test_that("scalar q, scalar params, non-symmetric", {
  p <- ptri(0.5,
            min = 0,
            max = 1,
            mode = 0.8)
  p_test <- ptri_test(0.5, 0, 1, 0.8)
  expect_equal(p, p_test)
})

test_that("scalar q, scalar params, non-symmetric, upper_tail", {
  p <- ptri(
    0.4,
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE
  )
  p_test <- ptri_test(0.4, 0, 1, 0.8)
  expect_equal(1 - p, p_test)
})

test_that("scalar q, scalar params, non-symmetric, log_p", {
  p <- ptri(
    0.5,
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  p_test <- ptri_test(0.5, 0, 1, 0.8)
  expect_equal(exp(p), p_test)
})

test_that("scalar q, scalar params, non-symmetric, upper_tail, log_p", {
  p <- ptri(
    0.4,
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE,
    log_p = TRUE
  )
  p_test <- ptri_test(0.4, 0, 1, 0.8)
  expect_equal(1 - exp(p), p_test)
})

test_that("vector q, scalar params, symmetric", {
  p <- ptri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.5)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0, 1, 0.5)
  expect_equal(p, p_test)
})

test_that("vector q, scalar params, non-symmetric", {
  p <- ptri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.8)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(p, p_test)
})

test_that("vector q, scalar params, non-symmetric, log_p", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(exp(p), p_test)
})

test_that("vector q, vector params, symmetric", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 2,
    mode = 1
  )
  p_test <- ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 2, 1)
  expect_equal(p, p_test)
})

test_that("vector q, vector params, non-symmetric", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(-0.1, 0, 0.1),
    max = 3,
    mode = 2
  )
  p_test <- ptri_vec_test(c(0.1, 0.6, 0.9), c(-0.1, 0, 0.1), 3, 2)
  expect_equal(p, p_test)
})

test_that("vector q, vector params, upper_tail", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.01, 0.02),
    max = 1,
    mode = 0.8,
    lower_tail = FALSE
  )
  p_test <-
    ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.01, 0.02), 1, 0.8)
  expect_equal(1 - p, p_test)
})

test_that("vector q, vector params, log_p", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.01, 0.02),
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  p_test <-
    ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.01, 0.02), 1, 0.8)
  expect_equal(exp(p), p_test)
})

test_that("vector q, vector params, upper_tail, log_p", {
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.01, 0.02),
    max = 1,
    mode = 0.8,
    lower_tail = FALSE,
    log_p = TRUE
  )
  p_test <-
    ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.01, 0.02), 1, 0.8)
  expect_equal(1 - exp(p), p_test)
})

test_that("Mode at bound, min == mode", {
  q <- c(0.1, 0.6, 0.9)
  p <- ptri(q,
            min = 0,
            max = 1,
            mode = 0)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0, 1, 0)
  expect_equal(p, p_test)
  p <- ptri(q,
            min = c(0, 0, 0),
            max = 1,
            mode = 0)
  p_test <- ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 1, 0)
  expect_equal(p, p_test)
})

test_that("Mode at bound, max == mode", {
  p <- ptri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 1)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0, 1, 1)
  expect_equal(p, p_test)
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 1,
    mode = 1
  )
  p_test <- ptri_vec_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 1, 1)
  expect_equal(p, p_test)
})

test_that("Zeros produced, q < min", {
  p <- ptri(c(0.1, 0.6, 0.9),
            min = 0.4,
            max = 1.4,
            mode = 0.9)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), 0.4, 1.4, 0.9)
  expect_equal(p, p_test)
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(0.4, 0.4, 0.4),
    max = 1.4,
    mode = 0.9
  )
  p_test <-
    ptri_vec_test(c(0.1, 0.6, 0.9), c(0.4, 0.4, 0.4), 1.4, 0.9)
  expect_equal(p, p_test)
})

test_that("Ones produced, q > max", {
  p <- ptri(c(0.1, 0.6, 0.9),
            min = -0.5,
            max = 0.5,
            mode = 0)
  p_test <- ptri_test(c(0.1, 0.6, 0.9), -0.5, 0.5, 0)
  expect_equal(p, p_test)
  p <- ptri(
    c(0.1, 0.6, 0.9),
    min = c(-0.5, -0.5, -0.5),
    max = 0.5,
    mode = 0
  )
  p_test <-
    ptri_vec_test(c(0.1, 0.6, 0.9), c(-0.5, -0.5, -0.5), 0.5, 0)
  expect_equal(p, p_test)
})

test_that("NaN produced, mode < min", {
  p <- expect_warning(ptri(
    1,
    min = 0,
    max = 2,
    mode = -1
  ))
  expect_equal(p, NaN)
  p <- expect_warning(ptri(
    c(1, 2, 3),
    min = c(-1, 0, 1),
    max = 3,
    mode = -1
  ))
  expect_equal(p, c(0.75, NaN, NaN))
})

test_that("NaN produced, min == mode == max", {
  p <- expect_warning(ptri(
    3,
    min = 3,
    max = 3,
    mode = 3
  ))
  expect_equal(p, NaN)
  p <- expect_warning(ptri(
    c(1, 2, 3),
    min = c(0, 1, 3),
    max = 3,
    mode = 3
  ))
  expect_equal(round(p, 7), c(0.1111111, 0.25, NaN))
})

test_that("NaN produced, min > max", {
  p <- expect_warning(ptri(
    1,
    min = 0,
    max = -1,
    mode = 1
  ))
  expect_equal(p, NaN)
  p <- expect_warning(ptri(
    c(1, 2, 3),
    min = c(1, 2, 3),
    max = 2.5,
    mode = 2
  ))
  expect_equal(p, c(0, 0, NaN))
})

test_that("Error, NULL arguments", {
  expect_error(ptri(q = NULL))
  expect_error(ptri(q = 1, min = NULL))
  expect_error(ptri(q = 1, max = NULL))
  expect_error(ptri(q = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(ptri(q = "1"))
  expect_error(ptri(q = 1, min = "0"))
  expect_error(ptri(q = 1, max = "1"))
  expect_error(ptri(q = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(ptri(q = 1, lower_tail = "TRUE"))
  expect_error(ptri(q = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(ptri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(ptri(
    seq(0.1, 1, 0.1),
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(ptri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})

################################################################################
## Test cases for the quantile function

test_that("scalar p, scalar params, symmetric", {
  q <- qtri(0.5,
            min = 0,
            max = 1,
            mode = 0.5)
  q_test <- qtri_test(0.5, 0, 1, 0.5)
  expect_equal(q, q_test)
})

test_that("scalar p, scalar params, non-symmetric", {
  q <- qtri(0.5,
            min = 0,
            max = 1,
            mode = 0.8)
  q_test <- qtri_test(0.5, 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("scalar p, scalar params, non-symmetric, upper_tail", {
  q <- qtri(
    0.4,
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE
  )
  q_test <- qtri_test(1 - 0.4, 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("scalar p, scalar params, non-symmetric, log_p", {
  q <- qtri(
    log(0.4),
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  q_test <- qtri_test(0.4, 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("scalar p, scalar params, non-symmetric, upper_tail, log_p", {
  q <- qtri(
    log(0.4),
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE,
    log_p = TRUE
  )
  q_test <- qtri_test(1 - 0.4, 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("vector p, scalar params, symmetric", {
  q <- qtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.5)
  q_test <- qtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.5)
  expect_equal(q, q_test)
})

test_that("vector p, scalar params, non-symmetric", {
  q <- qtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0.8)
  q_test <- qtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("vector p, scalar params, non-symmetric, log_p", {
  q <- qtri(
    log(c(0.1, 0.6, 0.9)),
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  q_test <- qtri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal(q, q_test)
})

test_that("vector p, vector params", {
  q <- qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.05, 0.1),
    max = 3,
    mode = 2
  )
  q_test <- qtri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.05, 0.1), 3, 2)
  expect_equal(q, q_test)
})

test_that("vector q, vector params, upper_tail", {
  q <- qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.05, 0.1),
    max = 3,
    mode = 2,
    lower_tail = FALSE
  )
  q_test <-
    qtri_vec_test(1 - c(0.1, 0.6, 0.9), c(0, 0.05, 0.1), 3, 2)
  expect_equal(q, q_test)
})

test_that("vector q, vector params, log_p", {
  q <- qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.05, 0.1),
    max = 3,
    mode = 2,
    log_p = FALSE
  )
  q_test <- qtri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.05, 0.1), 3, 2)
  expect_equal(q, q_test)
})

test_that("vector q, vector params, upper_tail, log_p", {
  q <- qtri(
    log(c(0.1, 0.6, 0.9)),
    min = c(0, 0.05, 0.1),
    max = 3,
    mode = 2,
    lower_tail = FALSE,
    log_p = TRUE
  )
  q_test <-
    qtri_vec_test(1 - c(0.1, 0.6, 0.9), c(0, 0.05, 0.1), 3, 2)
  expect_equal(q, q_test)
})

test_that("Mode at bound, min == mode", {
  q <- qtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 0)
  q_test <- qtri_test(c(0.1, 0.6, 0.9), 0, 1, 0)
  expect_equal(q, q_test)
  q <- qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 1,
    mode = 0
  )
  q_test <- qtri_vec_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 1, 0)
  expect_equal(q, q_test)
})

test_that("Mode at bound, max == mode", {
  q <- qtri(c(0.1, 0.6, 0.9),
            min = 0,
            max = 1,
            mode = 1)
  q_test <- qtri_test(c(0.1, 0.6, 0.9), 0, 1, 1)
  expect_equal(q, q_test)
  q <- qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.05, 0.1),
    max = 1,
    mode = 1
  )
  q_test <- qtri_vec_test(c(0.1, 0.6, 0.9), c(0, 0.05, 0.1), 1, 1)
  expect_equal(q, q_test)
})

test_that("NaN produced, p < 0 || p > 1", {
  q <- expect_warning(qtri(
    c(-0.5, 1.5),
    min = 0.4,
    max = 1.4,
    mode = 0.9
  ))
  expect_equal(q, c(NaN, NaN))
  q <- expect_warning(qtri(
    c(-0.5, 1.5),
    min = c(0.4, 0.1),
    max = 1.4,
    mode = 0.9
  ))
  expect_equal(q, c(NaN, NaN))
})

test_that("NaN produced, mode < min", {
  q <- expect_warning(qtri(
    c(0.4, 0.5),
    min = 0,
    max = 4,
    mode = -1
  ))
  expect_equal(q, c(NaN, NaN))
  q <- expect_warning(qtri(
    c(0.4, 0.5),
    min = 0,
    max = 1,
    mode = c(-1, 0.5)
  ))
  expect_equal(q, c(NaN, 0.5))
})

test_that("NaN produced, min == mode == max", {
  q <- expect_warning(qtri(
    c(0.4, 0.5),
    min = 0,
    max = 0,
    mode = 0
  ))
  expect_equal(q, c(NaN, NaN))
  q <- expect_warning(qtri(
    c(0, 0.5),
    min = 0,
    max = c(0, 1),
    mode = c(0, 0.5)
  ))
  expect_equal(q, c(NaN, 0.5))
})

test_that("NaN produced, min > max", {
  q <- expect_warning(qtri(
    c(0.4, 0.5),
    min = 0.5,
    max = 0,
    mode = 0.5
  ))
  expect_equal(q, c(NaN, NaN))
  q <- expect_warning(qtri(
    c(0, 0.5),
    min = c(1.5, 0),
    max = 1,
    mode = 0.5
  ))
  expect_equal(q, c(NaN, 0.5))
})

test_that("Error, NULL arguments", {
  expect_error(qtri(p = NULL))
  expect_error(qtri(p = 1, min = NULL))
  expect_error(qtri(p = 1, max = NULL))
  expect_error(qtri(p = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(qtri(p = "1"))
  expect_error(qtri(p = 1, min = "0"))
  expect_error(qtri(p = 1, max = "1"))
  expect_error(qtri(p = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(qtri(p = 1, lower_tail = "TRUE"))
  expect_error(qtri(p = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  expect_error(qtri(
    c(0.1, 0.6, 0.9),
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(qtri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(qtri(
    c(0.1, 0.6, 0.9),
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})

################################################################################
## Test cases for the random variate generator function

test_that("n == 1, scalar params, symmetric", {
  before_each()
  r <- rtri(1,
            min = 0,
            max = 1,
            mode = 0.5)
  r_test <- rtri_test(1, 0, 1, 0.5)
  expect_equal(r, r_test)
})

test_that("n == 1, scalar params, non-symmetric", {
  before_each()
  r <- rtri(1,
            min = 0,
            max = 1,
            mode = 0.8)
  r_test <- rtri_test(1, 0, 1, 0.8)
  expect_equal(r, r_test)
})

test_that("n > 1, scalar params, symmetric", {
  before_each()
  r <- rtri(3,
            min = 0,
            max = 1,
            mode = 0.5)
  r_test <- rtri_test(3, 0, 1, 0.5)
  expect_equal(r, r_test)
})

test_that("n > 1, scalar params, non-symmetric", {
  before_each()
  r <- rtri(3,
            min = 0,
            max = 1,
            mode = 0.8)
  r_test <- rtri_test(3, 0, 1, 0.8)
  expect_equal(r, r_test)
})

test_that("n > 1, vector params, symmetric", {
  before_each()
  r <- rtri(3,
            min = c(0, 1, 2),
            max = 3,
            mode = 2)
  r_test <- rtri_vec_test(3, c(0, 1, 2), 3, 2)
  expect_equal(r, r_test)
})

test_that("n > 1, vector params, non-symmetric", {
  before_each()
  r <- rtri(3,
            min = c(0.1, 0.6, 0.9),
            max = 4,
            mode = 3.8)
  r_test <- rtri_vec_test(3, c(0.1, 0.6, 0.9), 4, 3.8)
  expect_equal(r, r_test)
})

test_that("Mode at bound, min == mode", {
  before_each()
  r <- rtri(3,
            min = 0,
            max = 1,
            mode = 0)
  r_test <- rtri_test(3, 0, 1, 0)
  expect_equal(r, r_test)
  before_each()
  r <- rtri(3,
            min = c(0, 0, 0),
            max = 1,
            mode = 0)
  r_test <- rtri_vec_test(3, c(0, 0, 0), 1, 0)
  expect_equal(r, r_test)
})

test_that("Mode at bound, max == mode", {
  before_each()
  r <- rtri(3,
            min = 0,
            max = 1,
            mode = 1)
  r_test <- rtri_test(3, 0, 1, 1)
  expect_equal(r, r_test)
  before_each()
  r <- rtri(3,
            min = c(0.1, 0.5, 0.9),
            max = 1,
            mode = 1)
  r_test <- rtri_vec_test(3, c(0.1, 0.5, 0.9), 1, 1)
  expect_equal(r, r_test)
})

test_that("NaN produced, mode < min", {
  before_each()
  r <- expect_warning(rtri(
    2,
    min = 1,
    max = 2,
    mode = 0
  ))
  expect_equal(r, c(NaN, NaN))
  before_each()
  r <- expect_warning(rtri(
    2,
    min = c(1, 1),
    max = 2,
    mode = 0
  ))
  expect_equal(round(r, 7), c(NaN, NaN))
})

test_that("NaN produced, min == mode == max", {
  before_each()
  r <- expect_warning(rtri(
    2,
    min = 0,
    max = 0,
    mode = 0
  ))
  expect_equal(r, c(NaN, NaN))
  before_each()
  r <- expect_warning(rtri(
    2,
    min = 0,
    max = c(0, 0),
    mode = 0
  ))
  expect_equal(round(r, 7), c(NaN, NaN))
})

test_that("NaN produced, min > max", {
  before_each()
  r <- expect_warning(rtri(
    2,
    min = 0,
    max = -1,
    mode = 1
  ))
  expect_equal(r, c(NaN, NaN))
  before_each()
  r <- expect_warning(rtri(
    2,
    min = 0,
    max = c(-1, -1),
    mode = 1
  ))
  expect_equal(round(r, 7), c(NaN, NaN))
})

test_that("Error, Negative n", {
  before_each()
  expect_error(rtri(n = -1))
})

test_that("Error, NULL arguments", {
  before_each()
  expect_error(rtri(n = NULL))
  expect_error(rtri(n = 1, min = NULL))
  expect_error(rtri(n = 1, max = NULL))
  expect_error(rtri(n = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  before_each()
  expect_error(rtri(n = "1"))
  expect_error(rtri(n = 1, min = "0"))
  expect_error(rtri(n = 1, max = "1"))
  expect_error(rtri(n = 1, mode = "0.5"))
})

test_that("Error, illegal recycling", {
  before_each()
  expect_error(rtri(
    n = 10,
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(rtri(
    n = 10,
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(rtri(
    n = 10,
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})

################################################################################
## Test cases for the moment generating function

test_that("scalar t, scalar params, symmetric", {
  mg <- mgtri(0.5,
              min = 0,
              max = 1,
              mode = 0.5)
  mg_test <- mgtri_test(0.5, 0, 1, 0.5)
  expect_equal(mg, mg_test)
})

test_that("scalar t, scalar params, non-symmetric", {
  mg <- mgtri(0.5,
              min = 0,
              max = 1,
              mode = 0.8)
  mg_test <- mgtri_test(0.5, 0, 1, 0.8)
  expect_equal(mg, mg_test)
})

test_that("vector t, scalar params, symmetric", {
  mg <- mgtri(c(1, 2, 3),
              min = 0,
              max = 1,
              mode = 0.5)
  mg_test <- mgtri_test(c(1, 2, 3), 0, 1, 0.5)
  expect_equal(mg, mg_test)
})

test_that("vector t, scalar params, non-symmetric", {
  mg <- mgtri(c(1, 2, 3),
              min = 0,
              max = 1,
              mode = 0.8)
  mg_test <- mgtri_test(c(1, 2, 3), 0, 1, 0.8)
  expect_equal(mg, mg_test)
})

test_that("vector t, vector params, symmetric", {
  mg <- mgtri(
    c(1, 2, 3),
    min = c(0, 1, 2),
    max = c(2, 3, 4),
    mode = c(1, 2, 3)
  )
  mg_test <-
    mgtri_test(c(1, 2, 3), c(0, 1, 2), c(2, 3, 4), c(1, 2, 3))
  expect_equal(mg, mg_test)
})

test_that("vector t, vector params, non-symmetric", {
  mg <- mgtri(
    c(1, 2, 3),
    min = c(0, 1, 2),
    max = c(8, 9, 10),
    mode = c(7, 8, 9)
  )
  mg_test <-
    mgtri_test(c(1, 2, 3), c(0, 1, 2), c(8, 9, 10), c(7, 8, 9))
  expect_equal(mg, mg_test)
})

test_that("vector t, vector params recycled, symmetric", {
  mg <- mgtri(c(1, 2, 3),
              min = c(0, 0, 0),
              max = 2,
              mode = 1)
  mg_test <- mgtri_test(c(1, 2, 3), c(0, 0, 0), 2, 1)
  expect_equal(mg, mg_test)
})

test_that("vector t, vector params recycled, non-symmetric", {
  mg <- mgtri(c(1, 2, 3),
              min = c(0, 0.5, 1),
              max = 3,
              mode = 2)
  mg_test <- mgtri_test(c(1, 2, 3), c(0, 0.5, 1), 3, 2)
  expect_equal(mg, mg_test)
})

test_that("NaN produced, t == 0", {
  mg <- expect_warning(mgtri(
    c(0, 1, 2),
    min = 0.4,
    max = 1.4,
    mode = 0.9
  ))
  mg_test <- mgtri_test(c(0, 1, 2), 0.4, 1.4, 0.9)
  expect_equal(mg, mg_test)
  mg <- expect_warning(mgtri(
    c(0, 1, 2),
    min = c(0.4, 0.5, 0.6),
    max = 1.4,
    mode = 0.9
  ))
  mg_test <- mgtri_test(c(0, 1, 2), c(0.4, 0.5, 0.6), 1.4, 0.9)
  expect_equal(mg, mg_test)
})

test_that("NaN produced, Mode at bound, min == mode", {
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = 0,
    max = 1,
    mode = 0
  ))
  mg_test <- mgtri_test(c(1, 2, 3), 0, 1, 0)
  expect_equal(mg, mg_test)
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = c(0, 1, 2),
    max = c(3, 4, 5),
    mode = 2
  ))
  mg_test <- mgtri_test(c(1, 2, 3), c(0, 1, 2), c(3, 4, 5), 2)
  expect_equal(mg, mg_test)
})

test_that("NaN produced, Mode at bound, max == mode", {
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = 0,
    max = 1,
    mode = 1
  ))
  mg_test <- mgtri_test(c(1, 2, 3), 0, 1, 1)
  expect_equal(mg, mg_test)
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = c(0, 1, 2),
    max = c(3, 4, 5),
    mode = 3
  ))
  mg_test <- mgtri_test(c(1, 2, 3), c(0, 1, 2), c(3, 4, 5), 3)
  expect_equal(mg, mg_test)
})

test_that("NaN produced, mode < min", {
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = 0,
    max = 2,
    mode = -1
  ))
  expect_equal(mg, rep.int(NaN, 3))
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = c(0, 0.5, 1),
    max = 2,
    mode = -1
  ))
  expect_equal(mg, rep.int(NaN, 3))
})

test_that("NaN produced, min == mode == max", {
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = 2,
    max = 2,
    mode = 2
  ))
  expect_equal(mg, rep.int(NaN, 3))
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = c(0, 0.5, 1),
    max = 2,
    mode = 2
  ))
  expect_equal(mg, rep.int(NaN, 3))
})

test_that("NaN produced, min > max", {
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = 0,
    max = -1,
    mode = 1
  ))
  expect_equal(mg, rep.int(NaN, 3))
  mg <- expect_warning(mgtri(
    c(1, 2, 3),
    min = c(0, 0.5, 1),
    max = -1,
    mode = 1
  ))
  expect_equal(mg, rep.int(NaN, 3))
})

test_that("Error, NULL arguments", {
  expect_error(mgtri(t = NULL))
  expect_error(mgtri(t = 1, min = NULL))
  expect_error(mgtri(t = 1, max = NULL))
  expect_error(mgtri(t = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(mgtri(t = "1"))
  expect_error(mgtri(t = 1, min = "0"))
  expect_error(mgtri(t = 1, max = "1"))
  expect_error(mgtri(t = 1, mode = "0.5"))
})

test_that("Error, illegal recycling", {
  expect_error(mgtri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(mgtri(
    seq(0.1, 1, 0.1),
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(mgtri(
    seq(0.1, 1, 0.1),
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})

################################################################################
## Test cases for the expected shortfall function

test_that("scalar p, scalar params, symmetric", {
  es <- estri(0.5,
              min = 0,
              max = 1,
              mode = 0.5)
  es_test <- estri_test(0.5, 0, 1, 0.5)
  expect_equal2(es, es_test)
})

test_that("scalar p, scalar params, non-symmetric", {
  es <- estri(0.5,
              min = 0,
              max = 1,
              mode = 0.8)
  es_test <- estri_test(0.5, 0, 1, 0.8)
  expect_equal2(es, es_test)
})

test_that("scalar p, scalar params, non-symmetric, upper_tail", {
  es <- estri(
    0.4,
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE
  )
  es_test <- estri_test(1 - 0.4, 0, 1, 0.8)
  expect_equal2(es, es_test)
})

test_that("scalar p, scalar params, non-symmetric, log_p", {
  es <- estri(
    log(0.5),
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  es_test <- estri_test(0.5, 0, 1, 0.8)
  expect_equal2(es, estri_test(0.5, 0, 1, 0.8))
})

test_that("scalar p, scalar params, non-symmetric, upper_tail, log_p", {
  es <- estri(
    log(0.4),
    min = 0,
    max = 1,
    mode = 0.8,
    lower_tail = FALSE,
    log_p = TRUE
  )
  es_test <- estri_test(1 - 0.4, 0, 1, 0.8)
  expect_equal2(es, es_test)
})

test_that("vector p, scalar params, symmetric", {
  es <- estri(c(0.1, 0.6, 0.9),
              min = 0,
              max = 1,
              mode = 0.5)
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 0.5)
  expect_equal2(es, es_test)
})

test_that("vector p, scalar params, non-symmetric", {
  es <- estri(c(0.1, 0.6, 0.9),
              min = 0,
              max = 1,
              mode = 0.8)
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal2(es, es_test)
})

test_that("vector p, scalar params, non-symmetric, log_p", {
  p <- seq(0.1, 1, 0.1)
  es <- estri(
    log(c(0.1, 0.6, 0.9)),
    min = 0,
    max = 1,
    mode = 0.8,
    log_p = TRUE
  )
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 0.8)
  expect_equal2(es, es_test)
})

test_that("vector p, vector params, symmetric", {
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = c(2, 3, 4),
    mode = c(1, 2, 3)
  )
  es_test <-
    estri_test(c(0.1, 0.6, 0.9), c(0, 1, 2), c(2, 3, 4), c(1, 2, 3))
  expect_equal2(es, es_test)
})

test_that("vector p, vector params, non-symmetric", {
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(0, 1, 2),
    max = c(5, 6, 7),
    mode = c(4, 5, 6)
  )
  es_test <-
    estri_test(c(0.1, 0.6, 0.9), c(0, 1, 2), c(5, 6, 7), c(4, 5, 6))
  expect_equal2(es, es_test)
})

test_that("vector p, vector params, non-symmetric, upper_tail, log_p", {
  es <- estri(
    log(c(0.1, 0.6, 0.9)),
    min = c(0, 1, 2),
    max = c(5, 6, 7),
    mode = c(4, 5, 6),
    lower_tail = FALSE,
    log_p = TRUE
  )
  es_test <-
    estri_test(1 - c(0.1, 0.6, 0.9), c(0, 1, 2), c(5, 6, 7), c(4, 5, 6))
  expect_equal2(es, es_test)
})

test_that("vector p, vector params recycled, symmetric", {
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 1,
    mode = 0.5
  )
  es_test <- estri_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 1, 0.5)
  expect_equal2(es, es_test)
})

test_that("vector p, vector params recycled, non-symmetric", {
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(1, 2, 3),
    max = c(6, 7, 8),
    mode = c(5, 6, 7)
  )
  es_test <-
    estri_test(c(0.1, 0.6, 0.9), c(1, 2, 3), c(6, 7, 8), c(5, 6, 7))
  expect_equal2(es, es_test)
})

test_that("Mode at bound, min == mode", {
  es <- estri(c(0.1, 0.6, 0.9),
              min = 0,
              max = 1,
              mode = 0)
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 0)
  expect_equal2(es, es_test)
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 1,
    mode = 0
  )
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 0)
  expect_equal2(es, es_test)
})

test_that("Mode at bound, max == mode", {
  es <- estri(c(0.1, 0.6, 0.9),
              min = 0,
              max = 1,
              mode = 1)
  es_test <- estri_test(c(0.1, 0.6, 0.9), 0, 1, 1)
  expect_equal2(es, es_test)
  es <- estri(
    c(0.1, 0.6, 0.9),
    min = c(0, 0, 0),
    max = 1,
    mode = 1
  )
  es_test <- estri_test(c(0.1, 0.6, 0.9), c(0, 0, 0), 1, 1)
  expect_equal2(es, es_test)
})

test_that("NaN produced, p == 0 || p < 0 || p > 1", {
  p <- c(-0.01, 0, 1.01)
  es <- expect_warning(estri(
    p,
    min = 0.4,
    max = 1.4,
    mode = 0.9
  ))
  expect_equal(es, c(NaN, NaN, NaN))
  a <- c(0.3, 0.4, 0.5)
  es <- expect_warning(estri(
    p,
    min = a,
    max = 1.4,
    mode = 0.9
  ))
  expect_equal(es, c(NaN, NaN, NaN))
})

test_that("NaN produced, mode < min", {
  es <- expect_warning(estri(
    0.5,
    min = 0,
    max = 2,
    mode = -1
  ))
  expect_equal(es, NaN)
  es <- expect_warning(estri(
    c(0.5, 0.6),
    min = 0,
    max = 2,
    mode = c(-1, 1)
  ))
  expect_equal2(es, c(NaN, 0.7308565))
})

test_that("NaN produced, min == mode == max", {
  es <- expect_warning(estri(
    0.5,
    min = 0.5,
    max = 0.5,
    mode = 0.5
  ))
  expect_equal2(es, NaN)
  es <- expect_warning(estri(
    c(0.1, 0.5, 1),
    min = c(0, 1, 3),
    max = c(2, 3, 3),
    mode = c(1, 2, 3)
  ))
  expect_equal2(es, c(0.2981424, 1.6666667, NaN))
})

test_that("NaN produced, min > max", {
  es <- expect_warning(estri(
    0.5,
    min = 1,
    max = 0,
    mode = 0
  ))
  expect_equal(es, NaN)
  es <- expect_warning(estri(
    c(0, 0.5, 1),
    min = c(0, 1, 2),
    max = c(-1, 3, 4),
    mode = c(1, 2, 3)
  ))
  expect_equal2(es, c(NaN, 1.666667, 3))
})

test_that("Error, NULL arguments", {
  expect_error(estri(p = NULL))
  expect_error(estri(p = 1, min = NULL))
  expect_error(estri(p = 1, max = NULL))
  expect_error(estri(p = 1, mode = NULL))
})

test_that("Error, Non-numeric arguments", {
  expect_error(estri(p = "1"))
  expect_error(estri(p = 1, min = "0"))
  expect_error(estri(p = 1, max = "1"))
  expect_error(estri(p = 1, mode = "0.5"))
})

test_that("Error, Non-logical argument", {
  expect_error(estri(p = 1, lower_tail = "TRUE"))
  expect_error(estri(p = 1, log_p = "FALSE"))
})

test_that("Error, illegal recycling", {
  p <- seq(0.1, 1, 0.1)
  expect_error(estri(
    p,
    min = 0,
    max = c(1, 2),
    mode = 0.5
  ))
  expect_error(estri(
    p,
    min = c(0, 0.1),
    max = 1,
    mode = 0.5
  ))
  expect_error(estri(
    p,
    min = 0,
    max = 1,
    mode = c(0.5, 0.6)
  ))
})
