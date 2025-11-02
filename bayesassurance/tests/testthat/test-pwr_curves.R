library(testthat)
library(ggplot2)
library(bayesassurance)


## pwr_curves()
test_that("Assurance and power tables are of list type", {
  n <- seq(10, 200, 10)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_type(out$power_table, "list")
  expect_type(out$assurance_table, "list")
})


test_that("Calling simulated assurance table returns an error when 
          bayes_sim = FALSE", {
  n <- seq(10, 200, 10)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_true(is.null(out$bayes_sim_table))
  expect_true(is.null(out$mc_samples))
})


test_that("Correct row and column dimensions in assurance table", {
  n <- seq(10, 200, 10)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_equal(nrow(out$assurance_table), 20)
  expect_equal(ncol(out$assurance_table), 2)
})


test_that("Correct row and column dimensions in power table", {
  n <- seq(10, 200, 10)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_equal(nrow(out$power_table), 20)
  expect_equal(ncol(out$power_table), 2)
})


test_that("Dimensions of power and assurance tables match", {
  n <- seq(10, 200, 15)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_equal(nrow(out$power_table), nrow(out$assurance_table))
  expect_equal(ncol(out$power_table), ncol(out$assurance_table))
})


test_that("Plot output is a ggplot", {
  n <- seq(10, 200, 15)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_true(is.ggplot(out$plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(10, 200, 15)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = FALSE)
  expect_identical(out$plot$labels$y, "Power/Assurance")
  expect_identical(out$plot$labels$x, "Sample Size (n)")
})


test_that("Dimensions of assurance/power tables match with simulated 
          assurance table when bayes_sim = TRUE", {
  n <- seq(10, 200, 20)
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  out <- pwr_curve(n = n, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                   theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                   alpha = alpha, bayes_sim = TRUE)
  expect_equal(nrow(out$assurance_table), nrow(out$bayes_sim_table))
  expect_equal(nrow(out$power_table), nrow(out$bayes_sim_table))
  expect_equal(ncol(out$assurance_table), ncol(out$bayes_sim_table))
  expect_equal(ncol(out$power_table), ncol(out$bayes_sim_table))
})


test_that("Error message outputted for invalid n", {
  n_a <- 1e-8
  n_d <- 1e+8
  theta_0 <- 0.15
  theta_1 <- 0.25
  sigsq <- 0.104
  alpha <- 0.05
  expect_error(pwr_curve(n = 10, n_a = n_a, n_d = n_d, theta_0 = theta_0, 
                         theta_1 = theta_1, sigsq = sigsq, alt = "greater", 
                         alpha = alpha, bayes_sim = TRUE), 
               "length of n needs to be greater than 1.", 
               fixed=TRUE)
})
