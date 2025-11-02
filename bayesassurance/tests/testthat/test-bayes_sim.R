library(testthat)
library(ggplot2)
library(bayesassurance)


## bayes_sim() checks
test_that("Assurance table output is of list type", {
  n <- seq(100, 240, 20)

  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
  Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, sigsq = 0.265, mu_beta_d = 0.25,
  mu_beta_a = 0, alt = "greater", alpha = 0.05, mc_iter = 1000)
  
  expect_type(out$assurance_table, "list")
})


test_that("Correct row and column dimensions in table", {
  n <- seq(100, 240, 20)
  
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "two.sided", 
                                   alpha = 0.05, mc_iter = 1000)
  
  expect_equal(nrow(out$assurance_table), 8)
  expect_equal(ncol(out$assurance_table), 2)
})


test_that("Estimated assurance values are between 0 and 1", {
  n <- seq(100, 240, 20)
  
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "less", 
                                   alpha = 0.05, mc_iter = 1000)
  
  out$assurance_table$Prop.Ind <- ifelse(out$assurance_table$Assurance < 0 | 
                                      out$assurance_table$Assurance > 1, 0, 1)
  
  expect_equal(sum(out$assurance_table$Prop.Ind), nrow(out$assurance_table))
})


test_that("Assurance plot output is a ggplot", {
  n <- seq(100, 240, 20)
  
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "greater", 
                                   alpha = 0.05, mc_iter = 1000)
  
  expect_true(is.ggplot(out$assurance_plot))
})


test_that("Correct labels on x and y axes",{
  n <- seq(100, 240, 20)
  
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "greater", 
                                   alpha = 0.05, mc_iter = 1000)
  
  expect_identical(out$assurance_plot$labels$y, "Assurance")
  expect_identical(out$assurance_plot$labels$x, "Sample Size n")
})


test_that("Correct assurance value returned for scalar input of n", {
  n <- 100
  
  set.seed(1234)
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "greater", 
                                   alpha = 0.05, mc_iter = 1000)
  
  expect_equal(out$assur_val, "Assurance: 0.641")
})


test_that("No assurance plot and table produced for scalar input of n", {
  n <- 100
  
  out <- bayesassurance::bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = NULL,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "greater", 
                                   alpha = 0.05, mc_iter = 1000)
  
  expect_true(is.null(out$assurance_plot))
  expect_true(is.null(out$assurance_table))
})


# error message test
test_that("Error message outputted for mismatched dimensions of Xn and u", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = 1, u = c(1, 2), C = 0.15, Xn = Xn,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = 0.265, mu_beta_d = 0.25,
                         mu_beta_a = 0, alt = "greater", 
                         alpha = 0.05, mc_iter = 1000), 
               "Column dimension of Xn must be equal to row dimension of u.", 
               fixed=TRUE)
})



test_that("Error message outputted for mismatched dimensions of Xn 
          and mu_beta_d", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = Xn,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = 0.265, mu_beta_d = c(0.25, 0.3),
                         mu_beta_a = 0, alt = "less", 
                         alpha = 0.05, mc_iter = 1000), 
        "Column dimension of Xn must be equal to row dimension of mu_beta_d.", 
               fixed=TRUE)
})



test_that("Error message outputted for mismatched dimensions of Xn 
          and mu_beta_a", {
            n <- 100
            Xn <- bayesassurance::gen_Xn(n = 5)
            expect_error(bayes_sim(n, p = 1, u = 1, C = 0.15, Xn = Xn,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = c(0, 0), alt = "two.sided", 
                                   alpha = 0.05, mc_iter = 1000), 
        "Column dimension of Xn must be equal to row dimension of mu_beta_a.", 
                         fixed=TRUE)
})



test_that("Error message outputted for invalid C", {
            n <- 100
            Xn <- bayesassurance::gen_Xn(n = 5)
            expect_error(bayes_sim(n, p = 1, u = 1, C = c(0.2, 0.5), Xn = Xn,
                                   Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                                   sigsq = 0.265, mu_beta_d = 0.25,
                                   mu_beta_a = 0, alt = "greater", 
                                   alpha = 0.05, mc_iter = 1000), 
                         "C must be a scalar value.", fixed=TRUE)
})



test_that("Error message outputted for invalid sigsq", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = 1, u = 1, C = 0, Xn = Xn,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = c(0.2, 0.265), mu_beta_d = 0.25,
                         mu_beta_a = 0, alt = "greater", 
                         alpha = 0.05, mc_iter = 1000), 
               "sigsq must be scalar.", fixed=TRUE)
})



test_that("Error message outputted for invalid alpha", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = 1, u = 1, C = 0, Xn = Xn,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = 0.265, mu_beta_d = 0.25,
                         mu_beta_a = 0, alt = "greater", 
                         alpha = -0.05, mc_iter = 1000), 
        "Not a valid significance level, alpha must be between 0 and 1.", 
         fixed=TRUE)
})



test_that("Error message outputted for invalid specified alt", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = 1, u = 1, C = 0, Xn = Xn,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = 0.265, mu_beta_d = 0.25,
                         mu_beta_a = 0, alt = "greaterthan", 
                         alpha = 0.05, mc_iter = 1000), 
         "Please specify one of the three options for alternative test case: 
         greater, less, two.sided.", 
               fixed=TRUE)
})



test_that("Error message outputted for misspecified p", {
  n <- 100
  Xn <- bayesassurance::gen_Xn(n = 5)
  expect_error(bayes_sim(n, p = NULL, u = 1, C = 0, Xn = NULL,
                         Vbeta_d = 1e-8, Vbeta_a_inv = 0, Vn = NULL, 
                         sigsq = 0.265, mu_beta_d = 0.25,
                         mu_beta_a = 0, alt = "greater", 
                         alpha = 0.05, mc_iter = 1000), 
           "Need to specify column dimension of design matrix if design
           matrix wasn't specified in function call.", 
               fixed=TRUE)
})



