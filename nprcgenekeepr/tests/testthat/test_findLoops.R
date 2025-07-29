#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("findLoops")
library(testthat)
data("smallPed")
ped <- smallPed
data("smallPedTree")
smallLoops <- findLoops(smallPedTree)
test_that("findLoops correctly locates loops", {
  expect_false(all(sapply(smallLoops, function(item) item)))
  ped$sire[ped$id == "K"] <- "A"
  pedTree <- createPedTree(ped)
  loops <- findLoops(pedTree)
  expect_true(loops$L)
  expect_false(all(sapply(loops[!names(loops) == "L"], function(item) item)))
  ped$sire[ped$id == "K"] <- "Q"
  pedTree <- createPedTree(ped)
  loops <- findLoops(pedTree)
  expect_true(loops$L)
  expect_false(all(sapply(loops[!names(loops) == "L"], function(item) item)))
})
