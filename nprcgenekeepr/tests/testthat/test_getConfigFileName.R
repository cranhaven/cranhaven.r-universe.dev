#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getConfigFileName")

## These are just stub values
sysInfoUnix <-
  c(
    sysname = "Darwin",
    release = "17.7.0",
    version = paste0(
      "Darwin Kernel Version 17.7.0: Thu Jun 21 22:53:14 ",
      "PDT 2018; root:xnu-4570.71.2~1/RELEASE_X86_64"
    ),
    nodename = "prefect.local",
    machine = "x86_64",
    login = "rmsharp",
    user = "rmsharp", effective_user = "rmsharp"
  )
sysInfoWindows <-
  c(
    sysname = "Windows",
    release = "17.7.0",
    version = "Darwin Kernel Version 17.7.0: Thu Jun 21 22:53:14 PDT 2018; ",
    "root:xnu-4570.71.2~1/RELEASE_X86_64",
    nodename = "prefect.local",
    machine = "x86_64",
    login = "rmsharp",
    user = "rmsharp",
    effective_user = "rmsharp"
  )
test_that("getConfigFile got correct file name", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  expect_identical(
    getConfigFileName(sysInfoWindows)[["configFile"]],
    "/Users/rmsharp/_nprcgenekeepr_config"
  )
  expect_identical(
    getConfigFileName(sysInfoUnix)[["configFile"]],
    "/Users/rmsharp/.nprcgenekeepr_config"
  )
  expect_identical(
    getConfigFileName(sysInfoWindows)[["homeDir"]],
    "/Users/rmsharp"
  )
  expect_identical(
    getConfigFileName(sysInfoUnix)[["homeDir"]],
    "/Users/rmsharp"
  )
})
