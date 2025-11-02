library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_adcock() checks
test_that("Assurance table output is of list type", {
   n <- seq(20, 145, 5)
   out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                         n_a = 20, n_d = 10, sig_sq = 0.265,
                         alpha = 0.05, mc_iter = 1000)
  
  expect_type(out$assurance_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n <- seq(20, 145, 5)
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  expect_equal(nrow(out$assurance_table), 26)
  expect_equal(ncol(out$assurance_table), 2)
})


test_that("Estimated assurance values are between 0 and 1", {
  n <- seq(20, 145, 5)
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  out$assurance_table$Prop.Ind <- ifelse(out$assurance_table$Assurance < 0 | 
                                      out$assurance_table$Assurance > 1, 0, 1)
  
  expect_equal(sum(out$assurance_table$Prop.Ind), nrow(out$assurance_table))
})


test_that("Assurance plot output is a ggplot", {
  n <- seq(20, 145, 5)
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  expect_true(is.ggplot(out$assurance_plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(20, 145, 5)
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  expect_identical(out$assurance_plot$labels$y, "Assurance")
  expect_identical(out$assurance_plot$labels$x, "Sample Size n")
})


test_that("Correct assurance value returned for scalar input of n", {
  n <- 100
  
  set.seed(1234)
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  expect_equal(out$assur_val, "Assurance: 0.997")
})


test_that("No assurance plot and table produced for scalar input of n", {
  n <- 100
  
  out <- bayes_adcock(n = n, d = 0.20, mu_beta_a = 0.64, mu_beta_d = 0.9,
                      n_a = 20, n_d = 10, sig_sq = 0.265,
                      alpha = 0.05, mc_iter = 1000)
  
  expect_true(is.null(out$assurance_plot))
  expect_true(is.null(out$assurance_table))
})



# error message checks
test_that("Error message outputted for invalid precision d", {
  expect_error(bayes_adcock(n = 100, d = c(0.10, 0.20), mu_beta_a = 0.64, 
                            mu_beta_d = 0.9, n_a = 20, n_d = 10, 
                            sig_sq = 0.265, alpha = 0.05, mc_iter = 1000), 
               "Precision level d must be a scalar value.", 
               fixed=TRUE)
})



test_that("Error message outputted for invalid sig_sq", {
  expect_error(bayes_adcock(n = 100, d = 0.20, mu_beta_a = 0.64, 
                            mu_beta_d = 0.9, n_a = 20, n_d = 10, 
                            sig_sq = c(0.8, 0.265), alpha = 0.05, 
                            mc_iter = 1000), 
               "variance must be scalar value.", 
               fixed=TRUE)
})



test_that("Error message outputted for invalid n_a and n_d", {
  expect_error(bayes_adcock(n = 100, d = 0.20, mu_beta_a = 0.64, 
                            mu_beta_d = 0.9, n_a = c(10, 20), n_d = c(10, 15), 
                            sig_sq = 0.265, alpha = 0.05, mc_iter = 1000), 
               "Precision parameters n_a and n_d must be scalar values.", 
               fixed=TRUE)
})



test_that("Error message outputted for invalid alpha", {
  expect_error(bayes_adcock(n = 100, d = 0.20, mu_beta_a = 0.64, 
                            mu_beta_d = 0.9, n_a = 10, n_d = 100, 
                            sig_sq = 0.265, alpha = -0.05, mc_iter = 1000), 
               "Not a valid significance level, alpha must be between 0 and 1.", 
               fixed=TRUE)
})


