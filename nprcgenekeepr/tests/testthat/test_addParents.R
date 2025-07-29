#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addParents")

pedOne <- data.frame(
  id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedTwo <- data.frame(
  id = c(NA, "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedTwo <- pedTwo[!is.na(pedTwo$id), ]
pedThree <- data.frame(
  id = c("s1", "d1", "s2", NA, "o1", "o2", "o3", "o4"),
  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  stringsAsFactors = FALSE
)
pedThree <- pedThree[!is.na(pedThree$id), ]
pedFour <- data.frame(
  id = c("s1", NA, NA, "d2", "o1", "o2", "o3", "o4"),
  sire = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
  dam = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
  sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
  age = c(10.1, 11.2, 12.3, 13.4, 1.5, 2.6, 3.7, 4.8),
  stringsAsFactors = FALSE
)
pedFive <- pedFour[!is.na(pedFour$id), ]

test_that("addParents adds parents correctly", {
  newPed <- addParents(pedOne)
  expect_identical(nrow(pedOne), nrow(newPed)) # no change
  newPed <- addParents(pedTwo)
  expect_identical(nrow(pedTwo) + 1L, nrow(newPed)) # 1 sire added
  newPed <- addParents(pedThree)
  expect_identical(nrow(pedThree) + 1L, nrow(newPed)) # 1 dam added
  newPed <- addParents(pedFive)
  expect_identical(nrow(pedFive) + 2L, nrow(newPed)) # 1 sire and 1 dam added
})
# test_that("addParents adds parents correctly with numeric columns", {
#   newPed <- addParents(pedOne)
#   expect_equal(nrow(pedOne), nrow(newPed)) # no change
#   newPed <- addParents(pedTwo)
#   expect_equal(nrow(pedTwo) + 1L, nrow(newPed)) # 1 sire added
#   newPed <- addParents(pedThree)
#   expect_equal(nrow(pedThree) + 1L, nrow(newPed)) # 1 dam added
#   newPed <- addParents(pedFive)
#   expect_equal(nrow(pedFive) + 2L, nrow(newPed)) # 1 sire and 1 dam added
# })
test_that("addParents fails if a unknow column type is in the pedigree", {
  expect_error(addParents(cbind(pedTwo,
                                complx = runif(nrow(pedTwo)) * 1L + 4i)))
})
