#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("reportGV")
library(testthat)
qcPed <- nprcgenekeepr::qcPed
gvReport <- reportGV(qcPed, guIter = 100L)
test_that("reportGV forms correct genetic value report", {
  expect_named(gvReport, c(
    "report", "kinship", "gu", "fe", "fg",
    "maleFounders", "femaleFounders",
    "nMaleFounders", "nFemaleFounders", "total"
  ))
  expect_named(gvReport$report,
    c(
      "id", "sex", "age", "birth", "exit", "population",
      "indivMeanKin", "zScores", "gu", "totalOffspring",
      "livingOffspring", "value", "rank"
    )
  )
  expect_identical(nrow(gvReport$report), nrow(qcPed))
  expect_identical(nrow(gvReport$gu), nrow(qcPed))
  expect_identical(gvReport$nMaleFounders, 20L)
  expect_identical(gvReport$nFemaleFounders, 61L)
})
updateProgress <- function(n = 1L, detail = NULL, value = 0L, reset = FALSE) {
  "stub"
}

gvReport <- reportGV(qcPed, guIter = 100L, updateProgress = updateProgress)
test_that(
  "reportGV forms correct genetic value report with updateProgress defined",
  {
    expect_named(gvReport, c(
      "report", "kinship", "gu", "fe", "fg",
      "maleFounders", "femaleFounders",
      "nMaleFounders", "nFemaleFounders", "total"
    ))
    expect_named(gvReport$report,
      c(
        "id", "sex", "age", "birth", "exit", "population",
        "indivMeanKin", "zScores", "gu", "totalOffspring",
        "livingOffspring", "value", "rank"
      )
    )
    expect_identical(nrow(gvReport$report), nrow(qcPed))
    expect_identical(nrow(gvReport$gu), nrow(qcPed))
    expect_identical(gvReport$nMaleFounders, 20L)
    expect_identical(gvReport$nFemaleFounders, 61L)
  }
)
