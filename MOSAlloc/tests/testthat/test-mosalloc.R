# Filename: test-mosalloc.R
# Date: 31.12.2025
# Author: Felix Willems

# function: mosalloc()
test_that("mosalloc() works as expected for STRS", {

  # Infeasibility warning if sample size bounds are too large
  expect_warning(mosalloc(D = matrix(1, 1, 2),
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
  expect_warning(mosalloc(D = matrix(1, 1, 2),
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
               f = NULL, df = NULL, Hf = NULL,
               init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = TRUE)
  res <- mosalloc(D, d, A = NULL, a = NULL, C, c, l = 2, u = Nh, opts)

  expect_equal(sum(res$n), c)
  expect_equal(res$J, c(D %*% (1 / res$n) - d))
  expect_identical(all(round(res$n / (Nh * 0.1), 2) == 1), TRUE)

  # P1 with additional box constraint:
  # The parameter choice must lead to proportional allocation
  set.seed(123)
  Nh <- round(runif(10, 0.001, 1) * 1000)
  Sh <- rep(1, 10)
  D <- matrix((Nh * Sh)**2, 1)
  d <- as.vector(D %*% (1 / Nh))
  C <- matrix(rep(1, 10), 1)
  c <- as.vector(max(sum(Nh) * 0.1, 20))
  u <- Nh
  u[1] <- 2
  opts <- list(sense = "max_precision",
               f = NULL, df = NULL, Hf = NULL,
               init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = TRUE)
  nh <- mosalloc(D, d, A = NULL, a = NULL, C, c, l = 2, u, opts)$n

  expect_equal(sum(nh), c)
  vals <- nh / (Nh * 0.1)
  expect_identical(all(round(vals[-1] / mean(vals[-1]), 2) == 1), TRUE)

  # Cost minimization (error)
  set.seed(123)
  Nh <- round(runif(10, 0.001, 1) * 1000)
  Sh <- rep(1, 10)
  D <- matrix((Nh * Sh)**2, 1)
  d <- as.vector(D %*% (1 / Nh))
  C <- matrix(rep(1, 10), 1)
  c <- as.vector(max(sum(Nh) * 0.1, 20))
  opts <- list(sense = "min_cost", 
               f = NULL, df = NULL, Hf = NULL,
               init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = TRUE)
  expect_error(mosalloc(D, d, A = NULL, a = NULL, C, c, l = 2, Nh, opts),
               "No precision constraint specified!")

  propvar01 <- D %*% (1 / (Nh * 0.1) - 1 / Nh)

  A <- matrix((Nh * Sh)**2, 1)
  a <- as.vector(propvar01)
  D <- matrix(rep(1, 10), 1)
  d <- as.vector(0)
  res <- mosalloc(D, d, A, a, C = NULL, c = NULL, l = 2, Nh, opts)

  # Check optimal allocation (must be proportional to Nh)
  vals <- round(res$n / Nh * 0.1, 4)
  expect_equal(all(vals / vals[1] == 1), TRUE)

  # Check objective value
  expect_equal(res$J, c(C %*% res$n))

  # Check error message
  opts <- list(sense = "min_cost",
               f = function(x) sum(x**2),
               df = function(x) 2 * x,
               Hf = function(x) diag(2 * x**0),
               init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = TRUE)

  expect_error(mosalloc(D, d, A, a, C = NULL, c = NULL, l = 2, Nh, opts),
               "No multiobjective problem!")

  D <- matrix(c(rep(1, 10), 0.2, rep(1, 9)), 2)
  d <- as.vector(c(0, 0))
  expect_error(mosalloc(D, d, A, a, C = NULL, c = NULL, l = 2, Nh, opts),
               paste0("Minimization via decision functional not ",
                      "implemented for 'min_cost'!"))
})




test_that("mosalloc() works as expected for 2ST sampling", {
  # Check two-stage cluster sampling (comparison with theory, i.e.
  # only one overall cost constraint)
  set.seed(1234)
  pop <- data.frame(value = rnorm(100, 100, 35),
                    cluster = sample(1:4, 100, replace = TRUE))
  CI <- 27  # Sampling cost per PSU/cluster
  CII <- 10 # Average sampling cost per SSU
  NI <- 4                   # Number of PSUs/clusters
  NII <- table(pop$cluster) # PSU/cluster sizes
  S2I <- var(by(pop$value, pop$cluster, sum)) # between cluster variance
  S2II <- by(pop$value, pop$cluster, var)     # within cluster variances
  D <- matrix(c(NI**2 * S2I - NI * sum(NII * S2II), NI * NII**2 * S2II), 1)
  d <- as.vector(NI * S2I)
  C <- matrix(c(CI, rep(CII / NI, 4)), 1)
  c <- as.vector(500)
  l <- c(1, rep(1, 4))
  u <- c(NI, NI * NII)
  opts <- list(sense = "max_precision",
               f = NULL, df = NULL, Hf = NULL,
               init_w = 1, mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = TRUE)
  sol <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)

  # Theoretical solution (see Särndal, C.-E., Swenson, B., Wretman, J. 1993.
  # Model Assisted Survey Sampling. Springer. pp. 471-3)
  nII_opt_t <- NII * sqrt(NI * CI / (D[1, 1] / NI) * S2II / CII)
  nI_opt_t <- c / CI * (1 + sum(sqrt(S2II * NII**2 * CII / (CI * D[1, 1]))))^(-1)
  # CI * nI_opt_t + nI_opt_t / NI * sum(CII * nII_opt_t) # Must be [1] 500
  expect_equal(round(sol$n[1], 2), round(nI_opt_t, 2))
  expect_equal(round(sol$n[-1] / sol$n[1], 2),
               round(c(nII_opt_t %*% diag(4)), 2))
})