#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("summary.nprcgenekeeprErr")
library(testthat)
library(stringi)

pedOne <- nprcgenekeepr::pedOne
test_that("summary.nprcgenekeeprErr provides expected classes of output", {
  expect_type(summary(qcStudbook(pedOne, reportErrors = TRUE))$txt,
    "character"
  )
  expect_s3_class(summary(qcStudbook(pedOne, reportErrors = TRUE))$sp,
    "data.frame"
  )
})
test_that("summary.nprcgenekeeprErr provides expected output", {
  expect_length(summary(qcStudbook(pedOne,
                                             reportErrors = TRUE))$txt, 1L)
  expect_identical(nrow(summary(qcStudbook(pedOne,
                                           reportErrors = TRUE))$sp), 3L)
  expect_identical(stri_count_regex(
    summary(qcStudbook(pedOne, reportChanges = TRUE, reportErrors = TRUE))$txt,
    "\\n"
  ), 9L)
  pedTwo <- pedOne
  pedTwo$sex <- NULL
  expect_true(stri_detect_regex(
    summary(qcStudbook(pedTwo, reportChanges = TRUE, reportErrors = TRUE))$txt,
    pattern =
      "sex.\\n The required columns are: id, sire, dam, sex, and birth"
  ))
  pedTwo <- pedOne
  pedTwo$birth_date <- "badDate"
  expect_true(stri_detect_regex(
    summary(qcStudbook(pedTwo, reportChanges = TRUE, reportErrors = TRUE))$txt,
    pattern = stri_c(
      "There are 8 rows having an invalid date. ",
      "The first five records having bad dates are ",
      "on rows 1, 2, 3, 4, and 5."
    )
  ))
})
test_that(
  "summary.nprcgenekeeprErr identifies individual bad dates in date columns",
  {
    birth <- format(pedOne$birth_date, format = "%Y-%m-%d")
    birth[5L] <- "04-02-2015"
    birth[6L] <- "03-17-2009"
    pedEight <- pedOne
    pedEight$birth_date <- NULL
    pedEight$birth <- birth
    ped8 <- qcStudbook(pedEight, minParentAge = NULL, reportErrors = TRUE)
    summary(ped8)
    expect_true(stri_detect_fixed(
      summary(ped8)$txt,
      "rows having an invalid date are: 5 and 6"
    ))
  }
)
test_that("summary.nprcgenekeeprErr identifies bad database connection", {
  birth <- format(pedOne$birth_date, format = "%Y-%m-%d")
  birth[5L] <- "04-02-2015"
  birth[6L] <- "03-17-2009"
  pedEight <- pedOne
  pedEight$birth_date <- NULL
  pedEight$birth <- birth
  ped8 <- qcStudbook(pedEight, minParentAge = NULL, reportErrors = TRUE)
  ped8$failedDatabaseConnection <-
    "Database connection failed: configuration or permissions are invalid."
  summary(ped8)
  expect_true(stri_detect_fixed(
    summary(ped8)$txt,
    "Database connection failed"
  ))
})
