# Filename: test-mosallocSTRS.R
# Date: 03.01.2026
# Author: Felix Willems

# function: mosallocSTRS()
test_that("mosallocSTRS() works as expected for a simple univariate problem", {
  set.seed(123)
  # Problem data
  Nh <- round(runif(10, 0.001, 1) * 100) # stratum sizes
  Sh2 <- rep(1, 10) # stratum-specific variances
  Th <- rep(1, 10) # stratum-specific totals
  ch <- rep(2, 10) # stratum-specific sampling cost

  X_var <- matrix(Sh2, 10)
  X_tot <- matrix(Th, 10)

  # Objectives
  listD <- list(list(stratum_id = 1:10, variate = 1, measure = "relVAR",
                     name = "pop"))
  # Cost constraints
  listC <- list(list(stratum_id = 1:10, c_coef = ch, c_lower = NULL,
                     c_upper = 100, name = "max_budget"))
  # Specify stratum-specific box constraints
  l <- 1             # minimum sample size per stratum
  u <- Nh            # maximum sample size per stratum

  # Specify parameter for mosalloc (method = "WSS")
  opts <- list(sense = "max_precision",
               f = NULL, df = NULL, Hf = NULL,
               method = "WSS", init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = FALSE)

  # Solve allocation problem
  resWSS  <- mosallocSTRS(X_var, X_tot, Nh, listD, NULL, listC,
                          fpc = TRUE, l, u, opts)
  # Check computation
  expect_equal(sum(resWSS$n_opt), 50)
  expect_equal(c(ch %*% resWSS$n_opt), 100)
  expect_identical(resWSS$objectives[[1]] == c((Nh**2 / 10**2
                   ) %*% (1 / resWSS$n_opt) - (Nh**2 / 10**2
                   ) %*% (1 / Nh)), TRUE)
  # Specify parameter for mosalloc (method = "WCM")
  opts <- list(sense = "max_precision",
               f = NULL, df = NULL, Hf = NULL,
               method = "WCM", init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = FALSE)
  resWCM  <- mosallocSTRS(X_var, X_tot, Nh, listD, NULL, listC,
                          fpc = TRUE, l, u, opts)
  expect_equal(resWCM$n_opt, resWSS$n_opt)

  # Check parameter ForceOptimality for WCM
  resWCM_FO  <- mosallocSTRS(X_var, X_tot, Nh, listD, NULL, listC,
                             fpc = TRUE, l, u, opts, ForceOptimality = TRUE)
  expect_equal(resWCM_FO$n_opt, resWCM$n_opt)

  # Check summary function
  expect_identical(length(summary(resWSS)$objout), 6L)
  expect_identical(length(summary(resWCM)$objout), 5L)
  expect_identical(summary(resWSS)$method, "WSS")
  expect_identical(summary(resWCM)$method, "WCM")
})

test_that("mosallocSTRS() works as expected for a simple multivariate problem", {
  set.seed(123)
  # Problem data
  Nh <- round(runif(10, 0.001, 1) * 100) # stratum sizes
  # Construct stratum-specific variances and totals from a artificial population
  vals <- rnorm(sum(Nh), 1000, 500)
  X_var <- X_tot <- matrix(0, 10)
  for (i in 1:10) {
    bs <- cumsum(c(0, Nh))[c(i, i + 1)]
    X_var[i, 1] <- var(vals[(bs[1] + 1):bs[2]])
    X_tot[i, 1] <- sum(vals[(bs[1] + 1):bs[2]])
  }
  ch <- rep(1, 10) # stratum-specific sampling cost

  # Objectives
  listD <- list(list(stratum_id = 1:5, variate = 1, measure = "relVAR",
                     name = "Region1"),
                list(stratum_id = 6:10, variate = 1, measure = "relVAR",
                     name = "Region2"))
  # Cost constraints
  listA <- list(list(stratum_id = 1:10, variate = 1, measure = "RSE",
                     bound = 0.05, name = "pop"))
  # Cost constraints
  listC <- list(list(stratum_id = 1:10, c_coef = ch, c_lower = NULL,
                     c_upper = sum(Nh) * 0.14, name = "max_budget"))
  # Specify stratum-specific box constraints
  l <- 1             # minimum sample size per stratum
  u <- Nh            # maximum sample size per stratum

  # Specify parameter for mosalloc (method = "WCM")
  opts <- list(sense = "max_precision",
               f = NULL, df = NULL, Hf = NULL,
               method = "WCM", init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = FALSE)
  res  <- mosallocSTRS(X_var, X_tot, Nh, listD, listA, listC,
                       fpc = TRUE, l, u, opts)
  expect_equal(summary(res)$objout[1, 5], summary(res)$objout[2, 5])
})

test_that("mosallocSTRS works as expected for a cost optimization problem", {
  set.seed(123)
  # Problem data
  Nh <- round(runif(10, 0.001, 1) * 100) # stratum sizes
  vals <- rnorm(sum(Nh), 1000, 500)
  X_var <- X_tot <- matrix(0, 10)
  for (i in 1:10) {
    bs <- cumsum(c(0, Nh))[c(i, i + 1)]
    X_var[i, 1] <- var(vals[(bs[1] + 1):bs[2]])
    X_tot[i, 1] <- sum(vals[(bs[1] + 1):bs[2]])
  }
  X_cost <- matrix(1, 10) # stratum-specific sampling cost

  # Objectives to minimize maximum sample size
  listD <- list(list(stratum_id = 1:10, c_type = 1, name = "overall"))

  # Precision constraints
  listA <- list(list(stratum_id = 1:5, variate = 1, measure = "RSE",
                     bound = 0.05, name = "Region1"),
                list(stratum_id = 5:10, variate = 1, measure = "RSE",
                     bound = 0.05, name = "Region2"))
  # Specify stratum-specific box constraints
  l <- 1             # minimum sample size per stratum
  u <- Nh            # maximum sample size per stratum

  # Specify parameter for mosalloc (method = "WCM")
  opts <- list(sense = "min_cost",
               f = NULL, df = NULL, Hf = NULL,
               method = "WCM", init_w = 1,
               mc_cores = 1L, pm_tol = 1e-05,
               max_iters = 100L, print_pm = FALSE)

  # Solve allocation problem
  expect_error(mosallocSTRS(X_var, X_tot, Nh, listD, NULL, listC,
               fpc = TRUE, l, u, opts, X_cost = X_cost))
  res  <- mosallocSTRS(X_var, X_tot, Nh, listD, listA, NULL,
                       fpc = TRUE, l, u, opts, X_cost = X_cost)
  expect_equal(round(summary(res)$precision[["value"]], 5),
               summary(res)$precision[["bound"]])
})
