#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("calcFEFG")

data(lacy1989Ped)
data(lacy1989PedAlleles)
ped <- lacy1989Ped
alleles <- lacy1989PedAlleles
pedFactors <- data.frame(
  id = as.factor(ped$id),
  sire = as.factor(ped$sire),
  dam = as.factor(ped$dam),
  gen = ped$gen,
  population = ped$population,
  stringsAsFactors = TRUE
)
allelesFactors <- geneDrop(pedFactors$id, pedFactors$sire, pedFactors$dam,
  pedFactors$gen,
  genotype = NULL, n = 5000L,
  updateProgress = NULL
)
feFg <- calcFEFG(ped, alleles)
feFgFactors <- calcFEFG(pedFactors, allelesFactors)

## Prior to forcing the pedigree to have id, sire, and dam as character vectors
## inside calcFG, the two calculations above with ped (characters) and
## feFactors (factors) resulted 2.189855 and 1.857998 respectively.
##
## Used example from Analysis of Founder Representation in Pedigrees: Founder
## Equivalents and Founder Genome Equivalents.
## Zoo Biology 8:111-123, (1989) by Robert C. Lacy
## He presented 2.18 as the answer, which was truncated and not precise enough
## for this specific comparison.
test_that("calcFEFG correctly calculates the number of founder genetic
equivalents in the pedigree", {
  expect_lt(abs(feFg$FG - feFgFactors$FG), 0.2)
  expect_lt(abs(feFg$FG - 2.18), 0.2)
  expect_identical(feFg$FE, feFgFactors$FE)
  expect_equal(feFg$FE, 2.9090909091)
})
