# Filename: test-mosallocStepwiseFirst.R
# Date: 03.01.2026
# Author: Felix Willems

# function: mosallocStepwiseFirst()
test_that("mosallocStepwiseFirst() works as expected", {
  # Infeasibility warning if sample size bounds are too large
  expect_warning(mosallocStepwiseFirst(D = matrix(1, 1, 2),
                                       d = as.vector(0),
                                       A = NULL,
                                       a = null,
                                       C = matrix(1, 1, 2),
                                       c = as.vector(3),
                                       l = c(2, 2),
                                       u = c(3, 3),
                                       opts = list(sense = "max_precision",
                                                   init_w = 1,
                                                   mc_cores = 1L,
                                                   max_iters = 100L)),
                 "Allocation problem is infeasible!")

  # Infeasibility warning if precision and cost bound counteract
  expect_warning(mosallocStepwiseFirst(D = matrix(1, 1, 2),
                                       d = as.vector(0),
                                       A = matrix(c(1, 0), 1, 2),
                                       a = as.vector(1 / 3),
                                       C = matrix(1, 1, 2),
                                       c = as.vector(3),
                                       l = c(1, 1),
                                       u = c(3, 3),
                                       opts = list(sense = "max_precision",
                                                   init_w = 1,
                                                   mc_cores = 1L,
                                                   max_iters = 100L)),
                 "Allocation problem is infeasible!")

  # (P1) Simple univariate optimal allocation problem (precision maximization):
  # The parameter choice must lead to proportional allocation
  set.seed(123)
  Nh <- round(runif(10, 0.001, 1) * 1000)
  Sh <- rep(1, 10)
  D <- matrix((Nh * Sh)**2, 1)
  d <- as.vector(D %*% (1 / Nh))
  C <- matrix(rep(1, 10), 1)
  c <- as.vector(max(sum(Nh) * 0.1, 20))
  opts <- list(sense = "max_precision",
               init_w = 1,
               mc_cores = 1L,
               max_iters = 100L)
  res <- mosallocStepwiseFirst(D, d, A = NULL, a = NULL, C, c,
                                l = 2, u = Nh, opts)

  expect_equal(sum(res$n), c)
  expect_equal(res$J, c(D %*% (1 / res$n) - d))
  expect_identical(all(round(res$n / (Nh * 0.1), 2) == 1), TRUE)

  # P1 with additional box constraint:
  # The parameter choice must lead to proportional allocation
  u <- Nh
  u[1] <- 2
  opts <- list(sense = "max_precision",
               init_w = 1,
               mc_cores = 1L,
               max_iters = 100L)
  nh <- mosallocStepwiseFirst(D, d, A = NULL, a = NULL, C, c,
                              l = 2, u, opts)$n

  expect_equal(sum(nh), c)
  vals <- nh / (Nh * 0.1)
  expect_identical(all(round(vals[-1] / mean(vals[-1]), 2) == 1), TRUE)

  # (P2) Simple multivariate optimal allocation problem (precision
  # maximization): The parameter choice must lead to proportional allocation
  set.seed(123)
  Nh <- round(runif(10, 0.001, 1) * 1000)
  Sh <- c(rep(1, 19), 0.2)
  D <- matrix((Nh * Sh)**2, 2, byrow = TRUE)
  d <- as.vector(D %*% (1 / Nh))
  C <- matrix(rep(1, 10), 1)
  c <- as.vector(max(sum(Nh) * 0.1, 20))
  opts <- list(sense = "max_precision",
               init_w = 1,
               mc_cores = 1L,
               max_iters = 100L)
  res <- mosallocStepwiseFirst(D, d, A = NULL, a = NULL, C, c,
                               l = 2, u = Nh, opts)

  # Check proportional allocation
  expect_equal(all((round(res$n / (Nh * 0.1), 2) == 1)), TRUE)
  expect_equal(round(res$Normal[2], 2) == 0, TRUE)

  # Adjust parameter
  Sh <- c(0.2, rep(1, 19))
  D <- matrix((Nh * Sh)**2, 2, byrow = TRUE)
  d <- as.vector(D %*% (1 / Nh))
  res <- mosallocStepwiseFirst(D, d, A = NULL, a = NULL, C, c,
                               l = 2, u = Nh, opts)

  # Check proportional allocation
  expect_equal(all((round(res$n / (Nh * 0.1), 2) == 1)), TRUE)
  expect_equal(round(res$Normal[1], 2) == 0, TRUE)



  # Test error if opts$sense = min_cost (mosalloc_stepwise does not yet support
  # cost objectives, in particular, multiple cost objectives)
  set.seed(123)
  Nh <- round(runif(10, 0.001, 1) * 1000)
  Sh <- rep(1, 10)
  D <- matrix((Nh * Sh)**2, 1)
  d <- as.vector(D %*% (1 / Nh))
  C <- matrix(rep(1, 10), 1)
  c <- as.vector(max(sum(Nh) * 0.1, 20))
  opts <- list(sense = "min_cost",
               init_w = 1,
               mc_cores = 1L,
               max_iters = 100L)
  expect_error(mosallocStepwiseFirst(D, d, A = NULL, a = NULL, C, c,
                                     l = 2, Nh, opts),
               "Function not implemente for cost objectives!")
})