library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_sim_unknownvar() checks
test_that("No assurance plot produced for scalar n", {
  n <- 100
  p <- 1
  sigsq <- 4
  epsilon <- 10e-7
  a_sig_d <- (sigsq / epsilon) + 2
  b_sig_d <- sigsq * (a_sig_d - 1)
  a_sig_a <- -p / 2
  b_sig_a <- 0
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim_unknownvar(n = n, p = 1, 
         u = 1, C = 0, R = 50,
         Xn = NULL, Vn = NULL, Vbeta_d = 1e-8, 
         Vbeta_a_inv = 0, mu_beta_d = 0.25,
         mu_beta_a = 0, a_sig_a = a_sig_a, b_sig_a = b_sig_a, 
         a_sig_d = a_sig_d, b_sig_d = b_sig_d, 
         alt = "two.sided", alpha = 0.05, 
         mc_iter = 1000)
  expect_true(is.null(out$assur_plot))
})



test_that("Error produced for p = NULL and Xn = NULL", {
  n <- 100
  p <- 1
  sigsq <- 4
  epsilon <- 10e-7
  a_sig_d <- (sigsq / epsilon) + 2
  b_sig_d <- sigsq * (a_sig_d - 1)
  a_sig_a <- -p / 2
  b_sig_a <- 0
  
  expect_error(bayesassurance::bayes_sim_unknownvar(n = n, p = NULL, 
              u = 1, C = 0, R = 70,
              Xn = NULL, Vn = NULL, Vbeta_d = 1e-8, 
              Vbeta_a_inv = 0, mu_beta_d = 0.25,
              mu_beta_a = 0, a_sig_a = a_sig_a, b_sig_a = b_sig_a, 
              a_sig_d = a_sig_d, b_sig_d = b_sig_d, 
              alt = "less", alpha = 0.05, 
              mc_iter = 1000), 
              "Please specify column dimension p since Xn = NULL.", 
              fixed = TRUE)
})



test_that("Correct labels on x and y axes of assurance plot", {
  n <- seq(10, 50, 5)
  p <- 1
  sigsq <- 4
  epsilon <- 10e-7
  a_sig_d <- (sigsq / epsilon) + 2
  b_sig_d <- sigsq * (a_sig_d - 1)
  a_sig_a <- -p / 2
  b_sig_a <- 0
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim_unknownvar(n = n, p = 1, 
         u = 1, C = 0, R = 50,
         Xn = NULL, Vn = NULL, Vbeta_d = 1e-8, 
         Vbeta_a_inv = 0, mu_beta_d = 0.25,
         mu_beta_a = 0, a_sig_a = a_sig_a, b_sig_a = b_sig_a, 
         a_sig_d = a_sig_d, b_sig_d = b_sig_d, 
         alt = "greater", alpha = 0.05, 
         mc_iter = 1000)
  
  expect_identical(out$assur_plot$labels$y, "Assurance")
  expect_identical(out$assur_plot$labels$x, "Sample Size n")
})
