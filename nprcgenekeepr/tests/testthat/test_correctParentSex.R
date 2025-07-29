#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("correctParentSex")
library(testthat)
pedOne <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("F", "F", "M", "F", "F", "F", "F", "M"),
  recordStatus = rep("original", 8L),
  stringsAsFactors = FALSE
)
pedTwo <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c("d0", "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
  recordStatus = rep("original", 8L),
  stringsAsFactors = FALSE
)
pedThree <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c("d0", "d0", "d4", NA, "d1", "d2", "s1", "d2"),
  sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
  recordStatus = rep("original", 8L),
  stringsAsFactors = FALSE
)

pedOne$sex <- correctParentSex(
  pedOne$id, pedOne$sire, pedOne$dam, pedOne$sex,
  pedOne$recordStatus
)
pedTwo$sex <- correctParentSex(
  pedTwo$id, pedTwo$sire, pedTwo$dam, pedTwo$sex,
  pedOne$recordStatus
)
test_that("correctParentSex makes correct changes", {
  expect_true(pedOne$sex[1L] == "M")
  expect_true(pedTwo$sex[2L] == "F")
  expect_error(correctParentSex(
    pedThree$id, pedThree$sire, pedThree$dam,
    pedThree$sex, pedOne$recordStatus
  ))
})
test_that(paste0(
  "correctParentSex returns NULLs if no errors detected and ",
  "reportErrors flag is TRUE"
), {
  test <- correctParentSex(pedOne$id, pedOne$sire, pedOne$dam, pedOne$sex,
    pedOne$recordStatus,
    reportErrors = TRUE
  )
  expect_true(is.null(test$femaleSires) & is.null(test$maleDams))
  test <- correctParentSex(pedTwo$id, pedTwo$sire, pedTwo$dam, pedTwo$sex,
    pedOne$recordStatus,
    reportErrors = TRUE
  )
  expect_true(is.null(test$femaleSires) & is.null(test$maleDams))
})
test_that(paste0(
  "correctParentSex returns character vector with ID where ",
  "errors detected and reportErrors flag is TRUE"
), {
  expect_equal(correctParentSex(pedThree$id, pedThree$sire, pedThree$dam,
    pedThree$sex, pedOne$recordStatus,
    reportErrors = TRUE
  )$sireAndDam, "s1")
  pedTwo <- data.frame(
    id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
    sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
    dam = c("d0", "d0", "d4", NA, "d1", "d2", "d2", "d2"),
    sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
    stringsAsFactors = FALSE
  )
  expect_identical(correctParentSex(pedTwo$id, pedTwo$sire, pedTwo$dam, pedTwo$sex,
    pedOne$recordStatus,
    reportErrors = TRUE
  )$maleDams, "d1")
})
