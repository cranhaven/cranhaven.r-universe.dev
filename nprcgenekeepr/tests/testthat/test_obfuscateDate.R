#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("obfuscateDate")
library(testthat)

test_that("obfuscateDate creates new date within accepted range", {
  targetDate <- as.Date("2009-2-28", format = "%Y-%m-%d")
  minBirthDate <- as.Date("2009-2-14", format = "%Y-%m-%d")
  for (i in 1L:10L) {
    obDate <- obfuscateDate(targetDate, minBirthDate, 30L)
    expect_gte(obDate, as.Date("2009-02-14", format = "%Y-%m-%d"))
    expect_lte(obDate, as.Date("2009-03-30", format = "%Y-%m-%d"))
  }
})
test_that(
  "obfuscateDate creates new dates within accepted range with list of dates",
  {
    targetDate <- list(
      as.Date("2009-2-28", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    minBirthDate <- list(
      as.Date("2009-2-14", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    for (i in 1L:10L) {
      obDate <- obfuscateDate(targetDate, minBirthDate, 30L)
      expect_gte(obDate[[1L]], as.Date("2009-02-14", format = "%Y-%m-%d"))
      expect_lte(obDate[[1L]], as.Date("2009-03-30", format = "%Y-%m-%d"))
      expect_gte(obDate[[2L]], as.Date("2003-10-03", format = "%Y-%m-%d"))
      expect_lte(obDate[[2L]], as.Date("2003-11-02", format = "%Y-%m-%d"))
    }
  }
)
test_that(
  paste0(
    "obfuscateDate fails when length of minDate is not 1 or the ",
    "same as baseDate length."
  ),
  {
    targetDate <- list(
      as.Date("2009-2-28", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    minBirthDate <- list(
      as.Date("2009-2-14", format = "%Y-%m-%d")
    )
    expect_error(
      obfuscateDate(targetDate, minBirthDate, 10L),
      "Length of baseDate and minDate must be the same."
    )
    minBirthDate <- list(
      as.Date("2009-2-14", format = "%Y-%m-%d"),
      as.Date("2003-2-14", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    expect_error(
      obfuscateDate(targetDate, minBirthDate, 10L),
      "Length of baseDate and minDate must be the same."
    )
  }
)
test_that(
  paste0(
    "obfuscateDate fails when length of maxDelta is not 1 or the ",
    "same as baseDate length."
  ),
  {
    targetDate <- list(
      as.Date("2009-2-28", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    minBirthDate <- list(
      as.Date("2009-2-14", format = "%Y-%m-%d"),
      as.Date("2003-10-03", format = "%Y-%m-%d")
    )
    expect_error(
      obfuscateDate(targetDate, minBirthDate, c(10L, 20L, 30L)),
      "Length of minDate must be 1 or the same as baseDate."
    )
  }
)
