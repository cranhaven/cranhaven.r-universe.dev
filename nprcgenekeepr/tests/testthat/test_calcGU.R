#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("calcGU")
data("ped1Alleles")
test_that("calcGU forms dataframe with correct calculations", {
  gu_1 <- calcGU(ped1Alleles, threshold = 1L, byID = FALSE, pop = NULL)
  gu_3 <- calcGU(ped1Alleles, threshold = 3L, byID = FALSE, pop = NULL)
  expect_length(gu_1$gu[gu_1$gu == 50L], 110L)
  expect_length(gu_3$gu[gu_3$gu == 50L], 43L)
  gu_1 <- calcGU(ped1Alleles, threshold = 2L, byID = TRUE, pop = NULL)
  gu_3 <- calcGU(ped1Alleles,
    threshold = 3L, byID = FALSE,
    pop = ped1Alleles$id[20L:60L]
  )
  expect_length(gu_1$gu[gu_1$gu == 50L], 53L)
  expect_length(gu_3$gu[gu_3$gu == 50L], 0L)
})
