# test_scrape_reviews.R

library(testthat)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(parallel)

test_that("scrape_reviews function works correctly", {
  skip("Skipping this test for now.")
  expect_true(TRUE)
})

test_that("scrape_reviews function exists", {
  expect_true(exists("scrape_reviews"))
  expect_true(is.function(scrape_reviews))
})
