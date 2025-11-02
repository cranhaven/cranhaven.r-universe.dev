library(testthat)
library(ggplot2)
library(bayesassurance)


## assurance_nd_na()
test_that("Assurance table output is of list type", {
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.15, theta_1 = 0.25,
                         sigsq = 0.104, alt = "two.sided", alpha = 0.05)
  expect_type(out$assurance_table, "list")
})


test_that("Correct row and column dimensions in table", {
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.15, theta_1 = 0.25,
                  sigsq = 0.104, alt = "two.sided", alpha = 0.05)
  expect_equal(nrow(out$assurance_table), 49)
  expect_equal(ncol(out$assurance_table), 2)
})


test_that("Assurance plot output is a ggplot", {
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.15, theta_1 = 0.25,
                         sigsq = 0.104, alt = "two.sided", alpha = 0.05)
  expect_true(is.ggplot(out$assurance_plot))
})


test_that("Correct labels on x and y axes",{
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.15, theta_1 = 0.25,
                         sigsq = 0.104, alt = "two.sided", alpha = 0.05)
  expect_identical(out$assurance_plot$labels$y, "Assurance")
  expect_identical(out$assurance_plot$labels$x, "Sample Size n")
})


test_that("Error message resulting from viewing plots/tables for scalar 
          inputs of n", {
  out <- assurance_nd_na(n = 100, n_a = 1e-8, n_d = 1e+8, theta_0 = 0.15, 
                         theta_1 = 0.25, sigsq = 0.104, alt = "two.sided", 
                         alpha = 0.05)
  expect_error(out$assurance_plot)
  expect_error(out$assurance_table)
})


test_that("Assurance estimates are between 0 and 1", {
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.15, theta_1 = 0.25,
                         sigsq = 0.104, alt = "two.sided", alpha = 0.05)
  
  out$assurance_table$Prop.Ind <- ifelse(out$assurance_table$Assurance < 0 | 
                                        out$assurance_table$Assurance > 1, 0, 1)
  
  expect_equal(sum(out$assurance_table$Prop.Ind), nrow(out$assurance_table))
})


test_that("Correct string output given scalar n", {
  out <- assurance_nd_na(n = 100, n_a = 1e-8, n_d = 1e+8, theta_0 = 0.15, 
                         theta_1 = 0.25, sigsq = 0.104, alt = "two.sided", 
                         alpha = 0.05)
  expect_equal(out, "Assurance: 0.873")
})



test_that("Assurance table output is of list type", {
  out <- assurance_nd_na(n = seq(10, 250, 5), n_a = 1e-8, n_d = 1e+8, 
                         theta_0 = 0.25, theta_1 = 0.15,
                         sigsq = 0.104, alt = "less", alpha = 0.05)
  expect_type(out$assurance_table, "list")
})



test_that("Correct string output given scalar n", {
  out <- assurance_nd_na(n = 100, n_a = 1e-8, n_d = 1e+8, theta_0 = 0.25, 
                         theta_1 = 0.15, sigsq = 0.104, alt = "less", 
                         alpha = 0.05)
  expect_equal(out, "Assurance: 0.927")
})


# error message checks
test_that("Error message outputted for invalid n_a and n_d", {
  expect_error(assurance_nd_na(n = 100, n_a = c(10, 20), 
               n_d = c(10, 20), theta_0 = 0.25, 
               theta_1 = 0.15, sigsq = 0.104, alt = "less", 
               alpha = 0.05), 
               "n_a and n_d must be scalar quantites", fixed=TRUE)
})



test_that("Error message outputted for invalid theta_0", {
  expect_error(assurance_nd_na(n = 100, n_a = 1e-8, 
                               n_d = 1e+8, theta_0 = c(0.25, 0.24), 
                               theta_1 = c(0.15, 0.25), sigsq = 0.104, 
                               alt = "less", alpha = 0.05), 
               "theta_0 must be scalar", fixed=TRUE)
})



test_that("Error message outputted for invalid theta_1", {
  expect_error(assurance_nd_na(n = 100, n_a = 1e-8, 
                               n_d = 1e+8, theta_0 = 0.25, 
                               theta_1 = c(0.15, 0.25), sigsq = 0.104, 
                               alt = "less", alpha = 0.05), 
               "theta_1 must be scalar", fixed=TRUE)
})




test_that("Error message outputted for invalid sigsq (not positive)", {
  expect_error(assurance_nd_na(n = 100, n_a = 1e-8, 
                               n_d = 1e+8, theta_0 = 0.25, 
                               theta_1 = 0.15, sigsq = -1, 
                               alt = "less", alpha = 0.05), 
               "sigsq must be a positive scalar", fixed=TRUE)
})



test_that("Error message outputted for invalid alpha", {
  expect_error(assurance_nd_na(n = 100, n_a = 1e-8, 
                               n_d = 1e+8, theta_0 = 0.25, 
                               theta_1 = 0.15, sigsq = 0.104, alt = "less", 
                               alpha = -0.05), 
               "Not a valid significance level, alpha must be between 0 and 1.", 
               fixed=TRUE)
})




test_that("Error message outputted for invalid specified alt", {
  expect_error(assurance_nd_na(n = 100, n_a = 1e-8, 
                               n_d = 1e+8, theta_0 = 0.25, 
                               theta_1 = 0.15, sigsq = 0.104, alt = "lessthan", 
                               alpha = 0.05), 
            "Please specify one of the three options for alternative test case: 
         greater, less, two.sided.", 
               fixed=TRUE)
})

