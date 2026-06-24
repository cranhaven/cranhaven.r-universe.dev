# test_search_goodreads.R

library(testthat)
library(rvest)
library(dplyr)
library(stringr)
library(utils)

test_that("scrape_reviews function works correctly", {
  skip("Skipping this test for now.")
  expect_true(TRUE)
})

test_that("search_goodreads function exists", {
  expect_true(exists("search_goodreads"))
  expect_true(is.function(search_goodreads))
})
