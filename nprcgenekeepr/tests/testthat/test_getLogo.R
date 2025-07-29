#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getLogo")
logo <- suppressWarnings(getLogo())
test_that("getLogo returns reasonalble values", {
  expect_true(is.integer(logo$height))
  expect_true(is.integer(logo$width))
  expect_gt(logo$height, 0L)
  expect_gt(logo$width, 0L)
  expect_type(logo$file, "character")
})
sysInfo <- Sys.info()
homeDir <- paste0("~/")
configFile <- paste0(homeDir, ".nprcgenekeepr_config")
config <- c(homeDir = homeDir, configFile = configFile)
test_that("getLogo returns reasonalble values with SNPRC mock", {
  local_mocked_bindings(
    getSiteInfo = function() {
      list(
        center = "SNPRC",
        baseUrl = "http://deepthought:8080/labkey",
        schemaName = "study",
        folderPath = "/snprcEHR",
        queryName = "demographics",
        lkPedColumns = c(
          "Id",
          "gender",
          "birth",
          "death",
          "lastDayAtCenter",
          "dam",
          "sire"
        ),
        mapPedColumns = c("id", "sex", "birth", "death", "exit", "dam", "sire"),
        sysname = sysInfo[["sysname"]],
        release = sysInfo[["release"]],
        version = sysInfo[["version"]],
        nodename = sysInfo[["nodename"]],
        machine = sysInfo[["machine"]],
        login = sysInfo[["login"]],
        user = sysInfo[["user"]],
        effective_user = sysInfo[["effective_user"]],
        homeDir = config[["homeDir"]],
        configFile = config[["configFile"]]
      )
    }
  )
  logo <- suppressWarnings(getLogo())
  expect_type(logo$height, "integer")
  expect_type(logo$width, "integer")
  expect_gt(logo$height, 0L)
  expect_gt(logo$width, 0L)
  expect_type(logo$file, "character")
})
test_that("getLogo returns reasonalble values with ONPRC mock", {
  local_mocked_bindings(
    getSiteInfo = function() {
      list(
        center = "ONPRC",
        baseUrl = "https://primeuat.ohsu.edu",
        schemaName = "study",
        folderPath = "/ONPRC/EHR",
        queryName = "demographics",
        lkPedColumns = c(
          "Id",
          "gender",
          "birth",
          "death",
          "lastDayAtCenter",
          "Id/parents/dam",
          "Id/parents/sire"
        ),
        mapPedColumns = c("id", "sex", "birth", "death", "exit", "dam", "sire"),
        sysname = sysInfo[["sysname"]],
        release = sysInfo[["release"]],
        version = sysInfo[["version"]],
        nodename = sysInfo[["nodename"]],
        machine = sysInfo[["machine"]],
        login = sysInfo[["login"]],
        user = sysInfo[["user"]],
        effective_user = sysInfo[["effective_user"]],
        homeDir = config[["homeDir"]],
        configFile = config[["configFile"]]
      )
    }
  )
  logo <- suppressWarnings(getLogo())
  expect_type(logo$height, "integer")
  expect_type(logo$width, "integer")
  expect_gt(logo$height, 0L)
  expect_gt(logo$width, 0L)
  expect_type(logo$file, "character")
})
