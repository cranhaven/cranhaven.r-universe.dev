context("Functions messages")
library(andurinha)

test_that("findPeaks message when absMin < 0.1", {
  expect_message(findPeaks(andurinhaData, minAbs = 1.185))
})
