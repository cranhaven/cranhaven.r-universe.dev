#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("geneDrop")
set_seed(10L)
## This test is entirely dependent on repeatable pseudorandom sequence
## generation. If this is disturbed, it will need to be rewritten.
ped <- nprcgenekeepr::lacy1989Ped
nDrops <- 5L
pedFactors <- data.frame(
  id = as.factor(ped$id),
  sire = as.factor(ped$sire),
  dam = as.factor(ped$dam),
  gen = ped$gen,
  population = ped$population,
  stringsAsFactors = TRUE
)
genotype <- data.frame(
  id = ped$id,
  first_allele = c(
    NA, NA, "A001_B001", "A001_B002", NA,
    "A001_B002", "A001_B001"
  ),
  second_allele = c(
    NA, NA, "A010_B001", "A001_B001", NA,
    NA, NA
  ),
  stringsAsFactors = FALSE
)
pedWithGenotype <- addGenotype(ped, genotype)
pedGenotype <- getGVGenotype(pedWithGenotype)
allelesFactors <-
  geneDrop(
    pedFactors$id,
    pedFactors$sire,
    pedFactors$dam,
    pedFactors$gen,
    genotype = NULL,
    n = nDrops,
    updateProgress = NULL
  )
allelesNew <- geneDrop(
  ped$id,
  ped$sire,
  ped$dam,
  ped$gen,
  genotype = NULL,
  n = nDrops,
  updateProgress = NULL
)
allelesNewGen <- geneDrop(
  ped$id,
  ped$sire,
  ped$dam,
  ped$gen,
  genotype = pedGenotype,
  n = nDrops,
  updateProgress = NULL
)

test_that(
  "geneDrop correctly drops gene down the pedigree using
          random segregation by Mendelian rules",
  {
    expect_identical(table(as.numeric(allelesNew[7L, 1L:nDrops]))[[1L]], 1L)
    expect_identical(table(as.numeric(allelesNew[7L, 1L:nDrops]))[[2L]], 4L)
    expect_identical(
      table(as.numeric(allelesFactors[7L, 1L:nDrops]))[[1L]],
      2L
    )
    expect_identical(
      table(as.numeric(allelesFactors[7L, 1L:nDrops]))[[2L]],
      3L
    )
    expect_identical(
      table(as.numeric(allelesNewGen[7L, 1L:nDrops]))[["10001"]],
      nDrops
    )
    expect_identical(
      table(as.numeric(allelesNewGen[9L, 1L:nDrops]))[["10002"]],
      nDrops
    )
    expect_identical(
      table(as.numeric(allelesNewGen[13L, 1L:nDrops]))[["10001"]],
      5L
    )
    expect_identical(
      table(as.numeric(allelesNewGen[12L, 1L:nDrops]))[["6"]],
      3L
    )
  }
)
