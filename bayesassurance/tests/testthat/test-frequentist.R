library(testthat)
library(ggplot2)
library(bayesassurance)


## pwr_freq()
test_that("Power table output is of list type", {
  n <- seq(10, 140, 5)
  out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_type(out$pwr_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n <- seq(10, 140, 5)
  out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_equal(nrow(out$pwr_table), 27)
  expect_equal(ncol(out$pwr_table), 2)
})


test_that("Power plot output is a ggplot", {
  n <- seq(10, 140, 5)
  out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_true(is.ggplot(out$pwr_plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(10, 140, 5)
  out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_identical(out$pwr_plot$labels$y, "Power")
  expect_identical(out$pwr_plot$labels$x, "Sample Size n")
})


test_that("Error message resulting from viewing plots/tables for scalar 
          inputs of n", {
  out <- pwr_freq(n = 100, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_error(out$pwr_plot)
  expect_error(out$pwr_table)
})


test_that("Power estimates are between 0 and 1", {
  n <- seq(10, 140, 5)
  
  out <- pwr_freq(n = n, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  
  out$pwr_table$Prop.Ind <- ifelse(out$pwr_table$Power < 0 | 
                                     out$pwr_table$Power > 1, 0, 1)
  
  expect_equal(sum(out$pwr_table$Prop.Ind), nrow(out$pwr_table))
})


test_that("Power output is correct for `greater than` case for scalar n", {
  out <- pwr_freq(n = 100, theta_0 = 0.15, theta_1 = 0.35, sigsq = 0.3,
                  alt = "greater", alpha = 0.05)
  expect_equal(out, "Power: 0.978")
})


test_that("Power output is correct for `less than` case for scalar n", {
  out <- pwr_freq(n = 100, theta_0 = 0.25, theta_1 = 0.27, sigsq = 0.3,
                  alt = "less", alpha = 0.05)
  expect_equal(out, "Power: 0.022")
})


test_that("Power output is correct for `two.sided` case for scalar n
          (two sided case)", {
  out <- pwr_freq(n = 100, theta_0 = 0.25, theta_1 = 0.27, sigsq = 0.3,
                  alt = "two.sided", alpha = 0.05)
  expect_equal(out, "Power: 0.065")
})


test_that("Power table output is of list type (two sided case)", {
  n <- seq(10, 80, 10)
  out <- pwr_freq(n = n, theta_0 = 0.35, theta_1 = 0.55, sigsq = 0.3,
                  alt = "two.sided", alpha = 0.05)
  expect_type(out$pwr_table, "list")
})


test_that("Correct row and column dimensions in table (two sided case)", {
  n <- seq(10, 80, 10)
  out <- pwr_freq(n = n, theta_0 = 0.35, theta_1 = 0.55, sigsq = 0.3,
                  alt = "two.sided", alpha = 0.05)
  expect_equal(nrow(out$pwr_table), 8)
  expect_equal(ncol(out$pwr_table), 2)
})


test_that("Power output is correct for `two.sided` case for scalar n 
          (less than case)", {
  out <- pwr_freq(n = 100, theta_0 = 0.28, theta_1 = 0.25, sigsq = 0.3,
                  alt = "less", alpha = 0.05)
  expect_equal(out, "Power: 0.136")
})


test_that("Power table output is of list type 
          (less than case)", {
  n <- seq(10, 80, 10)
  out <- pwr_freq(n = n, theta_0 = 0.35, theta_1 = 0.14, sigsq = 0.3,
                  alt = "less", alpha = 0.05)
  expect_type(out$pwr_table, "list")
})


test_that("Correct row and column dimensions in table
          (less than case)", {
  n <- seq(10, 80, 10)
  out <- pwr_freq(n = n, theta_0 = 0.35, theta_1 = 0.55, sigsq = 0.3,
                  alt = "less", alpha = 0.05)
  expect_equal(nrow(out$pwr_table), 8)
  expect_equal(ncol(out$pwr_table), 2)
})


