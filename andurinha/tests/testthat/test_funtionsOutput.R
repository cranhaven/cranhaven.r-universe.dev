context("Functions output")
library(andurinha)

# ------------------------------------------------------------------
# findPeaks
test_that("findPeaks return a list", {
  expect_equal(class(findPeaks(andurinhaData)), "list")
  expect_equal(class(findPeaks(andurinhaData, scale = FALSE, ndd = FALSE)), "list")
})

test_that("finPeaks return a list with the correct length", {
  expect_equal(length(findPeaks(andurinhaData)), 4)
  expect_equal(length(findPeaks(andurinhaData, scale = FALSE)), 3)
  expect_equal(length(findPeaks(andurinhaData, ndd = FALSE)), 3)
  expect_equal(length(findPeaks(andurinhaData, scale = FALSE, ndd = FALSE)), 2)
})

# ------------------------------------------------------------------
# gOverview
test_that("gOverview return a ggplot objetc", {
  expect_equal(class(gOverview(andurinhaData)), c("gg", "ggplot"))
  expect_equal(class(gOverview(findPeaks(andurinhaData)$dataZ,
                     findPeaks(andurinhaData)$secondDerivative)), c("gg", "ggplot"))
})

# ------------------------------------------------------------------
# plotPeaks
test_that("plotPeaks return a ggplot objetc", {
  expect_equal(class(plotPeaks(findPeaks(andurinhaData)[[4]]$WN,
                               findPeaks(andurinhaData)$dataZ,
                               findPeaks(andurinhaData)$secondDerivative)), c("gg", "ggplot"))
  expect_equal(class(plotPeaks(findPeaks(andurinhaData, ndd =  FALSE)[[3]]$WN,
                               findPeaks(andurinhaData)$dataZ)), c("gg", "ggplot"))
})
