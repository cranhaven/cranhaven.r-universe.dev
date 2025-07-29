#' Copyright(c) 2017-2021 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addBackSecondParents")
uPedOne <- data.frame(
  id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedOne <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
uPedOne <- uPedOne[!is.na(uPedOne$id), ]
uPedTwo <- data.frame(
  id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, NA, NA, "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedTwo <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
uPedTwo <- uPedTwo[!is.na(uPedTwo$id), ]

uPedThree <- data.frame(
  id = c("s1", NA, "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, NA, "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedThree <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
uPedThree <- uPedThree[!is.na(uPedThree$id), ]
test_that("addBackSecondParents adds parents correctly", {
  newPed <- addBackSecondParents(uPedOne, pedOne)
  expect_identical(nrow(uPedOne), nrow(newPed)) # no change
  newPed <- addBackSecondParents(uPedTwo, pedTwo)
  expect_identical(nrow(uPedTwo) + 1L, nrow(newPed)) # "s1" added back
  newPed <- addBackSecondParents(uPedThree, pedThree)
  expect_identical(nrow(uPedThree) + 1L, nrow(newPed)) # "d1" added back
})
