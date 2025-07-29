#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
## context("makeGeneticDiversityDashboard")
## library(testthat)
##
## geneticDiversityStats <- data.frame(
##  group = c(paste0("Group_", 1:5), paste0("Corral_", 1:3)),
##  ## highLow = getProportionLow(geneticValues),
##  highLow = sample(1:3, 8, replace = TRUE),
##  ## indianOriginStatus = getIndianOriginStatus(origin)$colorIndex
##  indianOriginStatus = sample(1:3, 8, replace = TRUE),
##  ## fecundity = getProductionStatus(ped, minParentAge, maxOffspringAge,
##  ##                                 housing, currentDate)$colorIndex
##  fecundity = sample(1:3, 8, replace = TRUE),
##  kinshipWithMale = sample(1:3, 8, replace = TRUE),
##  genotypePhenotype = sample(1:3, 8, replace = TRUE))
##
## test_that("makeGeneticDiversityDashboard creates image file", {
## fn <- "~/tmp/dashboard_in_r.png"
## if (file.exists(fn))
##   file.remove(fn)
## expect_false(file.exists(fn))
## makeGeneticDiversityDashboard(geneticDiversityStats,
##                             file = fn)
## expect_true(file.exists(fn))
## if (file.exists(fn))
##   file.remove(fn)
## })
