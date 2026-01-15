context("Jack expansions")

test_that("Schur expansion", {
  skip_if_not_installed("jack", minimum_version = "6.0.0")
  library(jack)
  genpoch <- function(a, kappa, alpha) {
    prod(sapply(seq_along(kappa), function(i) {
      prod(a - (i - 1L) / alpha + seq_len(kappa[i]) - 1)
    }))
  }
  m <- 2
  alpha <- 1
  a <- c(2, 3)
  b <- c(4, 1i)
  x <- c(0.3i, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    SchurR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      SchurR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      SchurR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      SchurR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
  #
  a <- c(2, 3)
  b <- c(4, 5)
  x <- c(0.3, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    SchurR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      SchurR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      SchurR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      SchurR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
})

test_that("Zonal expansion", {
  skip_if_not_installed("jack", minimum_version = "6.0.0")
  library(jack)
  genpoch <- function(a, kappa, alpha) {
    prod(sapply(seq_along(kappa), function(i) {
      prod(a - (i - 1L) / alpha + seq_len(kappa[i]) - 1)
    }))
  }
  m <- 2
  alpha <- 2
  a <- c(2, 3)
  b <- c(4, 1i)
  x <- c(0.3i, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    ZonalR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      ZonalR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      ZonalR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      ZonalR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
  #
  a <- c(2, 3)
  b <- c(4, 5)
  x <- c(0.3, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    ZonalR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      ZonalR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      ZonalR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      ZonalR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
})

test_that("ZonalQ expansion", {
  skip_if_not_installed("jack", minimum_version = "6.0.0")
  library(jack)
  genpoch <- function(a, kappa, alpha) {
    prod(sapply(seq_along(kappa), function(i) {
      prod(a - (i - 1L) / alpha + seq_len(kappa[i]) - 1)
    }))
  }
  m <- 2
  alpha <- 1 / 2
  a <- c(2, 3)
  b <- c(4, 1i)
  x <- c(0.3i, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    ZonalQR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      ZonalQR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      ZonalQR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      ZonalQR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
  #
  a <- c(2, 3)
  b <- c(4, 5)
  x <- c(0.3, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    ZonalQR(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      ZonalQR(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      ZonalQR(x, c(1, 1)) / 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      ZonalQR(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha)
  expect_equal(o1, o2)
})

test_that("MSF expansion", {
  skip_if_not_installed("jack")
  library(jack)
  genpoch <- function(a, kappa, alpha) {
    prod(sapply(seq_along(kappa), function(i) {
      prod(a - (i - 1L) / alpha + seq_len(kappa[i]) - 1)
    }))
  }
  m <- 2
  alpha <- Inf
  a <- c(2, 3)
  b <- c(4, 1i)
  x <- c(0.3i, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    MSF(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      MSF(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      MSF(x, c(1, 1)) / 2 * 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      MSF(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha = 10000000)
  expect_equal(o1, o2, tolerance = 1e-7)
  #
  a <- c(2, 3)
  b <- c(4, 1)
  x <- c(0.3, 0.7)
  o1 <-
    genpoch(a[1], c(0), alpha) * genpoch(a[2], c(0), alpha) /
    genpoch(b[1], c(0), alpha) / genpoch(b[2], c(0), alpha) *
    MSF(x, c(0)) +
    genpoch(a[1], c(1), alpha) * genpoch(a[2], c(1), alpha) /
      genpoch(b[1], c(1), alpha) / genpoch(b[2], c(1), alpha) *
      MSF(x, c(1)) +
    genpoch(a[1], c(1, 1), alpha) * genpoch(a[2], c(1, 1), alpha) /
      genpoch(b[1], c(1, 1), alpha) / genpoch(b[2], c(1, 1), alpha) *
      MSF(x, c(1, 1)) / 2 * 2 +
    genpoch(a[1], c(2), alpha) * genpoch(a[2], c(2), alpha) /
      genpoch(b[1], c(2), alpha) / genpoch(b[2], c(2), alpha) *
      MSF(x, c(2)) / 2
  o2 <- hypergeomPFQ(m, a, b, x, alpha = 10000000)
  expect_equal(o1, o2, tolerance = 1e-7)
})
