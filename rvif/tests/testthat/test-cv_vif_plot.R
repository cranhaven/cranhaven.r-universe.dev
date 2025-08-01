# le pido a ChatGPT que prepare pruebas testthat para la siguiente función

library(testthat)
library(rvif)

#####################################################################################################################################

# Assuming that the cv_vif_plot() function is already loaded

# Simulation of a cv_vif result

set.seed(2025)
obs = 100
cte = rep(1, obs)
x2 = rnorm(obs, 5, 0.01)
x3 = rnorm(obs, 5, 10)
x4 = x3 + rnorm(obs, 5, 1)
x5 = rnorm(obs, -1, 30)
x = cbind(cte, x2, x3, x4, x5)
cv_vif_output = cv_vif(x)

test_that("Generate an error-free graph.", {
  expect_silent(cv_vif_plot(cv_vif_output))
})

# Simulation of a cv_vif result

cv_vif_output <- data.frame(
  CV = c(0.15, 0.22, 0.09),
  VIF = c(1.2, 3.5, 9.8)
)
rownames(cv_vif_output) <- c("Variable 2", "Variable 3", "Variable 4")

test_that("Generate an error-free graph.", {
  expect_silent(cv_vif_plot(cv_vif_output))
})

# 

test_that("Throws error with input that is not a data frame.", {
  x <- matrix(c(0.15, 1.2), ncol = 2)
  expect_silent(cv_vif_plot(x))  # It does not generate a message, but it also does not draw
})

test_that("Throws an error when the number of columns is not 2.", {
  bad_df <- data.frame(CV = c(0.1, 0.2, 0.3))
  expect_message(cv_vif_plot(bad_df), 
                 "The input is the output of the function 'cv_vif'")
})

test_that("No genera error aunque VIF tenga valores altos", {
  extreme_df <- data.frame(
    CV = c(0.1, 0.2, 0.3),
    VIF = c(15, 30, 100)
  )
  expect_silent(cv_vif_plot(extreme_df))
})

#####################################################################################################################################

# testthat::test_file("test-cv_vif_plot.R")