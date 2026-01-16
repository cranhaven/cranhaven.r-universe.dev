library(testthat)
library(checkmate)
library(tibble)
context("getColTypes")

test_that("get column types", {
  input <- tibble(c = "character",
                i = as.integer(1.1),
                n = as.numeric(1),
                d = as.double(1.11111111111111),
                l = TRUE,
                D = Sys.Date())

  out <- .getColTypes(input = input)
  expect_true(out == "cinnlD")
})

test_that("Error if arguments have wrong value", {

  expect_error(object = .getColTypes(input = "bla"))

})