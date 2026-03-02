## Copyright (C) 2024 Ching-Chuan Chen
##
## This file is part of RcppLbfgsBlaze.
##
## RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
## under the terms of the MIT License. You should have received
## a copy of MIT License along with RcppLbfgsBlaze.
## If not, see https://opensource.org/license/mit.

suppressPackageStartupMessages({
  require(Rcpp)
  require(RcppBlaze)
  require(RcppLbfgsBlaze)
  require(tinytest)
})

cppFile <- "test-lbfgs.cpp"
if (file.exists(file.path("cpp", cppFile))) {
  sourceCpp(file.path("cpp", cppFile))
} else {
  sourceCpp(system.file("tinytest", "cpp", cppFile, package = "RcppLbfgsBlaze"))
}

problem1_result <- test_problem1(c(-1.2, 1))
expect_equal(problem1_result$value, 0.0)
expect_equal(problem1_result$par, c(1, 1))
expect_equal(problem1_result$lbfgs_result_code, 0)

problem2_result <- test_problem2(c(-1.2, 1))
expect_equal(problem2_result$value, 0.0)
expect_equal(problem2_result$par, c(1, 3))
expect_equal(problem2_result$lbfgs_result_code, 0)

problem3_result <- test_problem3(10)
expect_equal(problem3_result$value, 0.0)
expect_equal(problem3_result$par, rep(1, 10))
expect_equal(problem3_result$lbfgs_result_code, 0)
