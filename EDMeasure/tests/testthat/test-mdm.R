context("mdm")

# random pairwise univariate
num_obs <- 10
dim_comp <- c(1, 1)
num_dim <- sum(dim_comp)
X <- matrix(rnorm(num_obs * num_dim), num_obs, num_dim)

test_that("dcov vs. asym_dcov/sym_dcov univariate", {
  m1 <- energy::dcov(X[, 1], X[, 2])^2
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m3 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat / 2

  expect_equal(m1, m2)
  expect_equal(m1, m3)
})

test_that("comp vs. asym_comp/sym_comp univariate", {
  m1 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m3 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat / 2

  expect_equal(m1, m2)
  expect_equal(m1, m3)
})

test_that("comp_simp vs. asym_comp_simp univariate", {
  m1 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat

  expect_equal(m1, m2)
})

# random pairwise multivariate
num_obs <- 10
dim_comp <- c(2, 2)
num_dim <- sum(dim_comp)
X <- matrix(rnorm(num_obs * num_dim), num_obs, num_dim)

test_that("dcov vs. asym_dcov/sym_dcov multivariate", {
  m1 <- energy::dcov(X[, 1:2], X[, 3:4])^2
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m3 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat / 2

  expect_equal(m1, m2)
  expect_equal(m1, m3)
})

test_that("comp vs. asym_comp/sym_comp multivariate", {
  m1 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m3 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat / 2

  expect_equal(m1, m2)
  expect_equal(m1, m3)
})

test_that("comp_simp vs. asym_comp_simp multivariate", {
  m1 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m2 <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat

  expect_equal(m1, m2)
})

# fixed pairwise univariate
num_obs <- 10
dim_comp <- c(1, 1)
num_dim <- sum(dim_comp)
X <- matrix(rep(1, num_obs * num_dim), num_obs, num_dim)

test_that("mdm pairwise univariate", {
  m <- rep(NA, 8)
  m[1] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m[2] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat
  m[3] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m[4] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m[5] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m[6] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat
  m[7] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat
  m[8] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp_simp")$stat

  expect_equal(all(m == 0), TRUE)
})

# fixed pairwise multivariate
num_obs <- 10
dim_comp <- c(2, 2)
num_dim <- sum(dim_comp)
X <- matrix(rep(1, num_obs * num_dim), num_obs, num_dim)

test_that("mdm pairwise multivariate", {
  m <- rep(NA, 8)
  m[1] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m[2] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat
  m[3] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m[4] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m[5] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m[6] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat
  m[7] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat
  m[8] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp_simp")$stat

  expect_equal(all(m == 0), TRUE)
})

# fixed mutual univariate
num_obs <- 10
dim_comp <- c(1, 1, 1)
num_dim <- sum(dim_comp)
X <- matrix(rep(1, num_obs * num_dim), num_obs, num_dim)

test_that("mdm mutual univariate", {
  m <- rep(NA, 8)
  m[1] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m[2] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat
  m[3] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m[4] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m[5] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m[6] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat
  m[7] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat
  m[8] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp_simp")$stat

  expect_equal(all(m == 0), TRUE)
})

# fixed mutual multivariate
num_obs <- 10
dim_comp <- c(2, 2, 2)
num_dim <- sum(dim_comp)
X <- matrix(rep(1, num_obs * num_dim), num_obs, num_dim)

test_that("mdm mutual multivariate", {
  m <- rep(NA, 8)
  m[1] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_dcov")$stat
  m[2] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_dcov")$stat
  m[3] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp")$stat
  m[4] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "comp_simp")$stat
  m[5] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp")$stat
  m[6] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "asym_comp_simp")$stat
  m[7] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp")$stat
  m[8] <- EDMeasure::mdm(X, dim_comp = dim_comp, type = "sym_comp_simp")$stat

  expect_equal(all(m == 0), TRUE)
})






