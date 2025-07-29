#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getSiteInfo")
library(stringi)
test_that("getSiteInfo at least returns the right elements", {
  expect_equal(
    suppressWarnings(names(getSiteInfo())),
    c(
      "center", "baseUrl", "schemaName", "folderPath", "queryName",
      "lkPedColumns", "mapPedColumns", "sysname", "release",
      "version", "nodename", "machine", "login", "user",
      "effective_user", "homeDir", "configFile"
    )
  )
})

test_that("getSiteInfo handled Windows and non-windows opperating systems", {
  siteInfo <- suppressWarnings(getSiteInfo())
  if (stri_detect_fixed(toupper(siteInfo$sysname), "WIND")) {
    expect_equal(
      siteInfo$homeDir,
      file.path(Sys.getenv("HOME"))
    )
    expect_equal(
      siteInfo$configFile,
      file.path(Sys.getenv("HOME"), "_nprcgenekeepr_config")
    )
  } else {
    expect_equal(siteInfo$homeDir, Sys.getenv("HOME"))
    expect_equal(siteInfo$configFile, file.path(Sys.getenv("HOME"),
                                                ".nprcgenekeepr_config"))
  }
})
test_that("getSiteInfo handle expectConfigFile parameter", {
  expect_warning(getSiteInfo())
  expect_warning(getSiteInfo(expectConfigFile = TRUE))
  expect_silent(getSiteInfo(expectConfigFile = FALSE))
})
