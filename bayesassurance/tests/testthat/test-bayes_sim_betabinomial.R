library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_betabinomial() checks
test_that("Assurance table output is of list type", {
  n <- seq(600, 700, 10)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
  alpha_1 = 0.5, beta_1 = 0.5, alpha_2 = 0.5, beta_2 = 0.5, sig_level = 0.05, 
  alt = "two.sided", mc_iter = 1000)

  expect_type(out$assurance_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n <- seq(600, 700, 10)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  expect_equal(nrow(out$assurance_table), 11)
  expect_equal(ncol(out$assurance_table), 3)
})


test_that("Estimated assurance values are between 0 and 1", {
  n <- seq(600, 700, 10)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  out$assurance_table$Prop.Ind <- ifelse(out$assurance_table$Assurance < 0 | 
                                      out$assurance_table$Assurance > 1, 0, 1)
  
  expect_equal(sum(out$assurance_table$Prop.Ind), nrow(out$assurance_table))
})


test_that("Assurance plot output is a ggplot", {
  n <- seq(600, 700, 10)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  expect_true(is.ggplot(out$assurance_plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(600, 700, 10)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  expect_identical(out$assurance_plot$labels$y, "Assurance")
  expect_identical(out$assurance_plot$labels$x, "Sample Size n = n1 = n2")
})


test_that("Correct assurance value returned for scalar input of n", {
  n <- 1000
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  expect_equal(out$assur_val, "Assurance: 0.748")
})



test_that("Correct assurance value returned for scalar input of n", {
  n <- 1000
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "greater", mc_iter = 1000)
  
  expect_equal(out$assur_val, "Assurance: 0.84")
})



test_that("Correct assurance value returned for scalar input of n", {
  n <- 1000
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.24, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "less", mc_iter = 1000)
  
  expect_equal(out$assur_val, "Assurance: 0.012")
})



test_that("No assurance plot and table produced for scalar input of n", {
  n <- 1000
  
  out <- bayesassurance::bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                           alpha_1 = 0.5, beta_1 = 0.5, 
                                           alpha_2 = 0.5, beta_2 = 0.5, 
                                           sig_level = 0.05, 
                                           alt = "two.sided", mc_iter = 1000)
  
  expect_true(is.null(out$assurance_plot))
  expect_true(is.null(out$assurance_table))
})



# error message tests

test_that("Error message outputted for different sized n1 and n2", {
  n1 <- c(10, 20, 30)
  n2 <- c(10, 20, 30, 40)
  expect_error(bayes_sim_betabin(n1 = n1, n2 = n2, p1 = 0.25, p2 = 0.2, 
                                 alpha_1 = 0.5, beta_1 = 0.5, 
                                 alpha_2 = 0.5, beta_2 = 0.5, 
                                 sig_level = 0.05, 
                                 alt = "two.sided", mc_iter = 1000), 
               "n1 and n2 must be of equal length.", 
               fixed=TRUE)
})




test_that("Error message outputted for invalid p1 and/or p2", {
  n <- c(10, 20, 30)
  
  expect_error(bayes_sim_betabin(n1 = n, n2 = n, p1 = c(0.25, 0.3), p2 = 0.2, 
                                 alpha_1 = 0.5, beta_1 = 0.5, 
                                 alpha_2 = 0.5, beta_2 = 0.5, 
                                 sig_level = 0.05, 
                                 alt = "two.sided", mc_iter = 1000), 
               "p1 and p2 must be scalar values.", 
               fixed=TRUE)
})



test_that("Error message outputted for invalid shape parameter(s)", {
  n <- c(10, 20, 30)
  
  expect_error(bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                 alpha_1 = c(0.5, 1), beta_1 = 0.5, 
                                 alpha_2 = 0.5, beta_2 = 0.5, 
                                 sig_level = 0.05, 
                                 alt = "two.sided", mc_iter = 1000), 
               "Shape parameters must be scalar values.", 
               fixed=TRUE)
})



test_that("Error message outputted for invalid significance level", {
  n <- c(10, 20, 30)
  
  expect_error(bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                 alpha_1 = 0.5, beta_1 = 0.5, 
                                 alpha_2 = 0.5, beta_2 = 0.5, 
                                 sig_level = -0.05, 
                                 alt = "two.sided", mc_iter = 1000), 
               "Not a valid significance level, must be between 0 and 1.", 
               fixed=TRUE)
})



test_that("Error message outputted for misspecified alternative", {
  n <- c(10, 20, 30)
  
  expect_error(bayes_sim_betabin(n1 = n, n2 = n, p1 = 0.25, p2 = 0.2, 
                                 alpha_1 = 0.5, beta_1 = 0.5, 
                                 alpha_2 = 0.5, beta_2 = 0.5, 
                                 sig_level = 0.05, 
                                 alt = "greatt", mc_iter = 1000), 
            "Please specify one of the three options for alternative test case: 
         greater, less, two.sided.", 
               fixed=TRUE)
})




