#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("assignAlleles")

alleles <- list(alleles = list(), counter = 1L)
test_that("assignAlleles assigns alleles correctly", {
  expect_error(assignAlleles(alleles,
    parentType = "sire", parent = "s1",
    id = "o1", n = 4L
  ))
  alleles_2 <- assignAlleles(alleles,
    parentType = "sire", parent = NA,
    id = "o1", n = 4L
  )
  expect_identical(alleles_2$alleles$o1$sire, rep(1L, 4L))
  alleles <- alleles_2
  alleles_3 <- assignAlleles(alleles,
    parentType = "dam", parent = NA,
    id = "o1", n = 4L
  )
  expect_identical(alleles_3$alleles$o1$dam, rep(2L, 4L))
  alleles <- alleles_3
  alleles_4 <- assignAlleles(alleles,
    parentType = "sire", parent = NA,
    id = "o2", n = 4L
  )
  expect_identical(alleles_4$alleles$o2$sire, rep(3L, 4L))
  alleles <- alleles_4
  alleles_5 <- assignAlleles(alleles,
    parentType = "dam", parent = NA,
    id = "o2", n = 4L
  )
  expect_identical(alleles_5$alleles$o2$dam, rep(4L, 4L))
  alleles <- alleles_5
  alleles_6 <- assignAlleles(alleles,
    parentType = "dam", parent = "o1",
    id = "o3", n = 4L
  )
  expect_true(all(alleles_6$alleles$o3$dam %in% c(1L, 2L)))
  alleles <- alleles_6
  alleles_7 <- assignAlleles(alleles,
    parentType = "dam", parent = "o2",
    id = "o3", n = 4L
  )
  expect_true(all(alleles_6$alleles$o3$sire %in% c(3L, 4L)))
})
