#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("createPedTree")
data("smallPed")
ped <- smallPed
pedTree <- createPedTree(ped)
test_that("createPedTree correctly breaks up pedigrees", {
  for (id in ped$id) {
    expect_identical(pedTree[[id]][["sire"]], ped$sire[ped$id == id])
    expect_identical(pedTree[[id]][["dam"]], ped$dam[ped$id == id])
  }
})
