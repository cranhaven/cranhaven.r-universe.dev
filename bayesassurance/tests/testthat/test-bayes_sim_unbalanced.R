library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_sim_unbalanced() checks
test_that("Assurance table output is of list type", {
  n1 <- seq(20, 40, 10)
  n2 <- seq(10, 30, 10)
  
  out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
  u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
  nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
  mu_beta_a = c(0, 0), alt = "two.sided", alpha = 0.05, mc_iter = 1000,
  surface_plot = TRUE)
  
  expect_type(out$assurance_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n1 <- seq(20, 40, 10)
  n2 <- seq(10, 30, 10)
  
  out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
  u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
  nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
  mu_beta_a = c(0, 0), alt = "greater", alpha = 0.05, mc_iter = 1000,
  surface_plot = TRUE)
  
  expect_equal(nrow(out$assurance_table), 3)
  expect_equal(ncol(out$assurance_table), 3)
})



test_that("No contour plot produced when surface_plot = FALSE", {
  n1 <- seq(20, 40, 10)
  n2 <- seq(10, 30, 10)
  
  out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
  u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
  nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
  mu_beta_a = c(0, 0), alt = "less", alpha = 0.05, mc_iter = 1000,
  surface_plot = FALSE)
  
  expect_true(is.null(out$contourplot))
})



test_that("No contour plot produced when n1 and n2 are scalars", {
  n1 <- 10
  n2 <- 20
  
  out <- bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
         u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
         nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
         Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
         mu_beta_a = c(0, 0), alt = "greater", alpha = 0.05, mc_iter = 1000,
         surface_plot = FALSE)
  
  expect_true(is.null(out$contourplot))
})



# error checks
test_that("Warning produced if n1 and n2 are scalars and 
          surface_plot = TRUE", {
  n1 <- 10
  n2 <- 20
  
  expect_warning(bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
  u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
  nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
  mu_beta_a = c(0, 0), alt = "greater", alpha = 0.05, mc_iter = 1000,
  surface_plot = TRUE), 
  "Can only set surface_plot = TRUE if n1 and n2 are vectors.", 
  fixed = TRUE)
})



test_that("Error produced if n1 and n2 are different lengths", {
  n1 <- c(10, 20)
  n2 <- c(10, 20, 30)
  
  expect_error(bayes_sim_unbalanced(n1 = n1, n2 = n2, repeats = 1, 
  u = c(1, -1), C = 0, Xn = NULL, Vbeta_d = matrix(c(50, 0, 0, 10),
  nrow = 2, ncol = 2), Vbeta_a_inv = matrix(rep(0, 4), nrow = 2, ncol = 2),
  Vn = NULL, sigsq = 100,  mu_beta_d = c(1.17, 1.25),
  mu_beta_a = c(0, 0), alt = "greater", alpha = 0.05, mc_iter = 1000,
  surface_plot = TRUE), 
  "n1 and n2 must be equal lengths.", 
  fixed = TRUE)
})
