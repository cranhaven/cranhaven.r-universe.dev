#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getRecordStatusIndex")
library(testthat)
test_that(
  "getRecordStatusIndex handles dataframe without a recordStatus column",
  {
    data("pedSix")
    expect_identical(nprcgenekeepr:::getRecordStatusIndex(pedSix), integer(0L))
    pedSix <- cbind(pedSix, recordStatus = c(
      rep("original", 5L),
      rep("added", 3L)
    ))
    expect_identical(
      nprcgenekeepr:::getRecordStatusIndex(pedSix, status = "added"),
      6L:8L
    )
    expect_identical(
      nprcgenekeepr:::getRecordStatusIndex(pedSix,
        status = "original"
      ),
      1L:5L
    )
  }
)
