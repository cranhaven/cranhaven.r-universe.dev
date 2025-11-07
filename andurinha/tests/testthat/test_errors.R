context("Functions errors")
library(andurinha)

# ------------------------------------------------------------------
# findPeaks
test_that("findPeaks errors when atribute missing/NA", {
  expect_error(findPeaks())
})

test_that("findPeaks errors when atribute NULL", {
  expect_error(findPeaks(NULL))
  expect_error(findPeaks(andurinhaData, resolution = NULL))
  expect_error(findPeaks(andurinhaData, minAbs = NULL))
  expect_error(findPeaks(andurinhaData, scale = NULL))
  expect_error(findPeaks(andurinhaData, ndd = NULL))
})

test_that("findPeaks errors when atribute character", {
  expect_error(findPeaks("1"))
  expect_error(findPeaks(andurinhaData, resolution = "1"))
  expect_error(findPeaks(andurinhaData, minAbs = "1"))
  expect_error(findPeaks(andurinhaData, cutOff = "1"))
  expect_error(findPeaks(andurinhaData, scale = "1"))
  expect_error(findPeaks(andurinhaData, ndd = "1"))
})

test_that("findPeaks errors when length > 1", {
  expect_error(findPeaks(andurinhaData, resolution = 1:3))
  expect_error(findPeaks(andurinhaData, minAbs = 1:3))
  expect_error(findPeaks(andurinhaData, cutOff = 1:3))
})

# ------------------------------------------------------------------
# gOverview
test_that("gOverview errors when abtribute missing/NA", {
  expect_error(gOverview())
  expect_error(gOverview(findPeaks(andurinhaData)$dataZ,
                         fontFamily = NA))
})

test_that("gOverview errors when abtribute NULL", {
  expect_error(gOverview(NULL))
  expect_error(gOverview(NULL, NULL))
})

test_that("gOverview errors when abtribute numeric", {
  expect_error(gOverview(1))
  expect_error(gOverview(findPeaks(andurinhaData)$dataZ, 1))
  expect_error(gOverview(findPeaks(andurinhaData)$dataZ, fontFamily = 1))
})

test_that("gOverview errors when abtribute character", {
  expect_error(gOverview("1"))
  expect_error(gOverview("1", "1"))
})

# ------------------------------------------------------------------
# plotPeaks
test_that("plorPeaks errors when abtribute missing/NA", {
  expect_error(plotPeaks())
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN,
                         findPeaks(andurinhaData)$dataZ,
                         fontFamily = NA))
})

test_that("plorPeaks errors when abtribute NULL", {
  expect_error(plotPeaks(NULL))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, NULL, NULL))
})

test_that("plorPeaks errors when abtribute numeric", {
  expect_error(plotPeaks(1))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, data_abs = 1))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, data_ndd = 1))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, fontFamily = 1))
})

test_that("plorPeaks errors when abtribute character", {
  expect_error(plotPeaks("1"))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, data_abs = "1"))
  expect_error(plotPeaks(findPeaks(andurinhaData)[[4]]$WN, data_ndd = "1"))
})


