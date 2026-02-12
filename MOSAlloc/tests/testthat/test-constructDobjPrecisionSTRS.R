# Filename: test-constructDobjPrecisionSTRS.R
# Date: 31.12.2025
# Author: Felix Willems

# function: constructDobjPrecisionSTRS()
test_that("constructDobjPrecisionSTRS() works as expected)", {

  N <- c(6221, 11738, 4333, 22809, 5467) # stratum sizes

  # Revenues
  mh.rev <- c(85, 11, 23, 17, 126) # mean revenue
  Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0) # standard deviation revenue

  # Employees
  mh.emp <- c(511, 21, 70, 32, 157) # mean number of employees
  Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00) # std. dev. employees

  # Matrix containing stratum-specific variance components
  X_var <- cbind(Sh.rev**2,
                 Sh.emp**2)

  # Matrix containing stratum-specific totals
  X_tot <- cbind(mh.rev, mh.emp) * N
  colnames(X_tot) <- c("rev", "emp")

  # test
  list <- list(list(stratum_id = 1:5, variate = 1, measure = "VAR",
                    name = "pop"))
  expect_identical(is.list(constructDobjPrecisionSTRS(X_var, X_tot, N,
                                                      list, fpc = TRUE)), TRUE)
  list <- list(list(stratum_id = 1:5, variate = "rev", measure = "VAR",
                    name = "pop"))
  expect_error(constructDobjPrecisionSTRS(X_var, X_tot, N, list, fpc = TRUE))

  colnames(X_var) <- c("rev", "emp")
  expect_identical(is.list(constructDobjPrecisionSTRS(X_var, X_tot, N,
                                                      list, fpc = TRUE)), TRUE)

  list <- list(list(stratum_id = 1:5, variate = "rev", measure = "VAR",
                    name = "pop"),
               list(stratum_id = 1:3, variate = "emp", measure = "VAR",
                    name = "pop"))
  Dd <- constructDobjPrecisionSTRS(X_var, X_tot, N, list, fpc = TRUE)

  # domain names + number
  expect_identical(rownames(Dd$D), names(Dd$d))
  expect_equal(nrow(Dd$D), 2)
  expect_identical(all(unname(Dd$D[2, 4:5]) == c(0, 0)), TRUE)

  # values
  expect_equal((Dd$D %*% (1 / N))[, 1], Dd$d)
  val <- t(X_var * N**2)
  val[2, 4:5] <- 0
  colnames(val) <- 1:5
  rownames(val) <- paste0(rownames(val),
                          paste0("_", unlist(lapply(list, function(L)L$name))))
  expect_equal(Dd$D, val)

  # test 'fpc= FALSE'
  expect_equal(unname(constructDobjPrecisionSTRS(X_var, X_tot, N,
                                                 list, fpc = FALSE)$d),
               c(0, 0))

  # test 'list[[*]]$measure = relVar'
  list <- list(list(stratum_id = 1:5, variate = "rev", measure = "relVAR",
                    name = "pop"),
               list(stratum_id = 1:3, variate = "emp", measure = "relVAR",
                    name = "pop"))
  Dd2 <- constructDobjPrecisionSTRS(X_var, X_tot, N, list, fpc = TRUE)

  totals <- X_tot
  totals[4:5, 2] <- 0
  expect_equal(Dd2$D, Dd$D / colSums(totals)**2)
})