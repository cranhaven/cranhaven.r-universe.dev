#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("filterKinMatrix")

ped <- nprcgenekeepr::qcPed
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
  sparse = FALSE
)
ids <- ped$id[c(189L, 192L, 194L, 195L)]
ncols <- ncol(kmat)
nrows <- nrow(kmat)
kmatFiltered <- filterKinMatrix(ids, kmat)
test_that("filterKinMatrix retains the correct rows and columns", {
  expect_equal(kmatFiltered[1L, 2L], kmat[189L, 192L])
  expect_equal(kmatFiltered[1L, 3L], kmat[189L, 194L])
  expect_equal(kmatFiltered[1L, 4L], kmat[189L, 195L])
  expect_equal(kmatFiltered[2L, 3L], kmat[192L, 194L])
})
ids <- c(
  "C1ICXL", "2KULR3", "RI0O7F", "7M51X5", "170ZTZ", "Y7PPEZ",
  "CFPEEU", "ZC5SCR", "218FOV", "2IXJ2N", "CAST4W", "JGPN6K", "HOYW0S",
  "DD1U77", "0DAV0I", "HLI95R", "TZ5NUB", "DR5GXB", "EUG3WE", "FHV13N",
  "OUM6QF", "6Z7MD9", "309VM2", "8KM1MP", "I9TQ0T", "INGWI7"
)

kmatFiltered <- filterKinMatrix(ids, kmat)
test_that("filterKinMatrix leaves the correct rows", {
  expect_length(ids, nrow(kmatFiltered))
  expect_length(ids, ncol(kmatFiltered))
  expect_identical(
    kmat[
      (seq_len(nrow(kmat)))[rownames(kmat) %in% ids[20L:23L]],
      (seq_len(ncol(kmat)))[colnames(kmat) %in% ids[20L:23L]]
    ],
    kmatFiltered[20L:23L, 20L:23L]
  )
})
