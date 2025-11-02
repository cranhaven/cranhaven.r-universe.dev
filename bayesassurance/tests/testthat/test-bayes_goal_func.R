library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_goal_func() checks
test_that("Assurance table output is of list type", {
  n <- seq(100, 300, 10)
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
  sigsq = 1, u = 1, beta_0 = 0.5, beta_1 = 0.6)
  
  expect_type(out$rc_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n <- seq(100, 300, 10)
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  expect_equal(nrow(out$rc_table), 21)
  expect_equal(ncol(out$rc_table), 2)
})


test_that("Estimated rate of correct classification values are 
          between 0 and 1", {
  n <- seq(100, 300, 10)
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  out$rc_table$Prop.Ind <- ifelse(out$rc_table$`Rate of Correct Classification` 
                           < 0 | out$rc_table$`Rate of Correct Classification` 
                           > 1, 0, 1)
  
  expect_equal(sum(out$rc_table$Prop.Ind), nrow(out$rc_table))
})


test_that("Rate of correct classification plot is a ggplot", {
  n <- seq(100, 300, 10)
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  expect_true(is.ggplot(out$rc_plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(100, 300, 10)
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  expect_identical(out$rc_plot$labels$y, "Rate of Correct Classification")
  expect_identical(out$rc_plot$labels$x, "Sample Size n")
})


test_that("Correct value returned for scalar input of n", {
  n <- 100
  
  set.seed(1234)
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  expect_equal(out$rc_val, "Rate of Correct Classification: 0.691")
})


test_that("No plot and table produced for scalar input of n", {
  n <- 100
  
  out <- bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
                                         sigsq = 1, u = 1, beta_0 = 0.5, 
                                         beta_1 = 0.6)
  
  expect_true(is.null(out$rc_plot))
  expect_true(is.null(out$rc_table))
})


# error checks 
test_that("Expect error for mismatched dimensions of Xn and beta_0", {
  n <- c(5, 10, 15)
  Xn <- gen_Xn(n = n)
  
  expect_error(bayesassurance::bayes_goal_func(n, Xn = Xn, K = 1, pi = 0.5,
  sigsq = 1, u = 1, beta_0 = 0.5, beta_1 = 0.6), 
  "Dimension mismatch. Check design matrix.", fixed = TRUE)
})


test_that("Expect error for mismatched dimensions of beta_0 and beta_1", {
  n <- c(5, 10, 15)
  
  expect_error(bayesassurance::bayes_goal_func(n, Xn = NULL, K = 1, pi = 0.5,
              sigsq = 1, u = 1, beta_0 = c(0.5, 0.6), beta_1 = 0.6), 
              "beta_0 and beta_1 must have equal dimensions.", fixed = TRUE)
})


test_that("No plot and table produced for scalar input of n", {
  n <- c(5, 10, 15)
  Xn <- gen_Xn(n = n)
  
  expect_error(bayesassurance::bayes_goal_func(n, Xn = Xn, K = 1, pi = 0.5,
               sigsq = 1, u = c(1, -1), beta_0 = c(0.5, 0.6, 0.7), 
               beta_1 = c(0.5, 0.6, 0.7)), 
           "Dimension mismatch. Make sure beta_0, beta_1, and u
           have equal dimensions.", fixed = TRUE)
})





