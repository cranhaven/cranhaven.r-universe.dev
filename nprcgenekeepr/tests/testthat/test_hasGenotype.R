#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("hasGenotype")
library(testthat)
library(stringi)
genotype <- data.frame(
  id = stri_c(2500L + 1L:20L),
  first = 10000L + 1L:20L,
  second = 11000L + 1L:20L,
  stringsAsFactors = FALSE
)

test_that("hasGenotype ensures correct dataframe", {
  expect_true(hasGenotype(genotype))
  genotype <- data.frame(
    id = stri_c(2500L + 1L:20L),
    first = 10000L + 1L:20L,
    second_name = stri_c("sec:ond_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    id = stri_c(2500L + 1L:20L),
    first = 10000L + 1L:20L,
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    first = 10000L + 1L:20L,
    second = 11000L + 1L:20L,
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    first = 10000L + 1L:20L,
    second = 11000L + 1L:20L,
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_true(hasGenotype(genotype))
  expect_false(hasGenotype(genotype[, 1L:2L]))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    first = as.character(10000L + 1L:20L),
    second = 11000L + 1L:20L,
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    first = 10000L + 1L:20L,
    second = as.character(11000L + 1L:20L),
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_false(hasGenotype(genotype))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    first = as.numeric(10000L + 1L:20L),
    second = 11000L + 1L:20L,
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_true(hasGenotype(genotype))
  genotype <- data.frame(
    ego = stri_c(2500L + 1L:20L),
    id = stri_c(2500L + 1L:20L),
    first_name = stri_c("first_name", 1L:20L),
    first = 10000L + 1L:20L,
    second = as.numeric(11000L + 1L:20L),
    second_name = stri_c("second_name", 1L:20L),
    stringsAsFactors = FALSE
  )
  expect_true(hasGenotype(genotype))
})
