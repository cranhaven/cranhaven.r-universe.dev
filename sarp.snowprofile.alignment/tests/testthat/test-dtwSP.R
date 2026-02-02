context("Profile alignments")
library(sarp.snowprofile.alignment)

test_that("resampled, uneven (nL_q > nL_r), bottom.up (i.e., layer-tracking)", {
  Arel <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
                checkGlobalAlignment = FALSE, bottom.up = TRUE, top.down = FALSE, keep.internals = TRUE,
                dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                window.size = 0.3)
  expect_gte(simSP(Arel$reference, Arel$queryWarped), 0)
  expect_equal(Arel$localCostMatrix[289, 221], 0)
  expect_equal(Arel$imin, 186)
  expect_equal(Arel$imin, Arel$index1[length(Arel$index1)])
  expect_equal(Arel$queryWarped$layers$hardness, Arel$query$layers$hardness[Arel$queryWarped$layers$queryLayerIndex])
})
test_that("resampled, uneven (nL_q > nL_r), top.down", {
  Arel <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
                checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
                dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                window.size = 0.3)
  expect_gte(simSP(Arel$reference, Arel$queryWarped), 0)
  expect_equal(Arel$localCostMatrix[289, 221], 0)
  expect_equal(Arel$imin, 2)
  expect_equal(Arel$imin, Arel$index1[length(Arel$index1)])
  expect_equal(Arel$queryWarped$layers$hardness, Arel$query$layers$hardness[Arel$queryWarped$layers$queryLayerIndex])
})

test_that("resampled, uneven (nL_q < nL_r), bottom.up (i.e., layer-tracking)", {
  Arelrev <- dtwSP(SPpairs$C_day1, SPpairs$C_day3, resamplingRate = 0.5, rescale2refHS = FALSE,
                   checkGlobalAlignment = FALSE, bottom.up = TRUE, top.down = FALSE, keep.internals = TRUE,
                   dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                   window.size = 0.3)
  expect_gte(simSP(Arelrev$reference, Arelrev$queryWarped), 0)
  expect_equal(Arelrev$localCostMatrix[221, 289], 0)
  expect_equal(Arelrev$jmin, 186)
  expect_equal(Arelrev$jmin, Arelrev$index2[length(Arelrev$index2)])
  expect_equal(Arelrev$queryWarped$layers$hardness, Arelrev$query$layers$hardness[Arelrev$queryWarped$layers$queryLayerIndex])
  })
test_that("resampled, uneven (nL_q < nL_r), top.down", {
  Arelrev <- dtwSP(SPpairs$C_day1, SPpairs$C_day3, resamplingRate = 0.5, rescale2refHS = FALSE,
                   checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
                   dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                   window.size = 0.3)
  expect_gte(simSP(Arelrev$reference, Arelrev$queryWarped), 0)
  expect_equal(Arelrev$localCostMatrix[221, 289], 0)
  expect_equal(Arelrev$jmin, 2)
  expect_equal(Arelrev$jmin, Arelrev$index2[length(Arelrev$index2)])
  expect_equal(Arelrev$queryWarped$layers$hardness, Arelrev$query$layers$hardness[Arelrev$queryWarped$layers$queryLayerIndex])
})

test_that("rescaled and resampled, top.down", {
  B <- dtwSP(SPpairs$C_day1, SPpairs$C_day3, resamplingRate = 0.5, rescale2refHS = TRUE,
                   checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
                   dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                   window.size = 0.3)
  expect_gte(simSP(B$reference, B$queryWarped), 0)
  expect_equal(dim(B$localCostMatrix)[1], dim(B$localCostMatrix)[2])
  expect_equal(B$queryWarped$hs, B$query$hs)
  expect_equal(B$imin, 3)
  expect_equal(B$imin, B$index1[length(B$index1)])
  expect_equal(B$queryWarped$layers$hardness, B$query$layers$hardness[B$queryWarped$layers$queryLayerIndex])

  Brev <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = TRUE,
             checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
             dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
             window.size = 0.3)
  expect_gte(simSP(Brev$reference, Brev$queryWarped), 0)
  expect_equal(Brev$jmin, 3)
  expect_equal(Brev$jmin, Brev$index2[length(Brev$index2)])
  expect_equal(Brev$queryWarped$layers$hardness, Brev$query$layers$hardness[Brev$queryWarped$layers$queryLayerIndex])
})


test_that("resampled, uneven, best match", {
  C <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
                checkGlobalAlignment = TRUE, bottom.up = TRUE, top.down = TRUE, keep.internals = TRUE,
                dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                window.size = 0.3)
  expect_gte(simSP(C$reference, C$queryWarped), 0)
  expect_equal(dim(C$localCostMatrix), c(289, 221))
  expect_equal(C$localCostMatrix[289, 221], 0)
  expect_match(C$openEndType_verbose, "matched_full_sequences")
  expect_equal(C$jmin, 221)
  expect_lt(abs(C$queryWarped$hs - C$reference$hs), 0.5)
  expect_equal(C$queryWarped$layers$hardness, C$query$layers$hardness[C$queryWarped$layers$queryLayerIndex])

  Crev <- dtwSP(SPpairs$C_day1, SPpairs$C_day3, resamplingRate = 0.5, rescale2refHS = FALSE,
                checkGlobalAlignment = TRUE, bottom.up = TRUE, top.down = TRUE, keep.internals = TRUE,
                dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                window.size = 0.3)
  expect_gte(simSP(Crev$reference, Crev$queryWarped), 0)
  expect_equal(dim(Crev$localCostMatrix), c(221, 289))
  expect_equal(Crev$localCostMatrix[221, 289], 0)
  expect_match(Crev$openEndType_verbose, "matched_full_sequences")
  expect_equal(Crev$jmin, 289)
  expect_gt(Crev$queryWarped$hs, Crev$query$hs)
  expect_equal(Crev$queryWarped$layers$hardness, Crev$query$layers$hardness[Crev$queryWarped$layers$queryLayerIndex])

  D <- dtwSP(SPpairs$D_generalAlignment1, SPpairs$D_generalAlignment2, resamplingRate = 0.5, rescale2refHS = FALSE,
                 checkGlobalAlignment = TRUE, bottom.up = TRUE, top.down = TRUE, keep.internals = TRUE,
                 dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
                 window.size = 0.3)
  expect_gte(simSP(D$reference, D$queryWarped), 0)
  expect_match(D$openEndType_verbose, "matched_full_sequences")
  expect_equal(D$jmin, 292)
  expect_equal(D$queryWarped$layers$hardness, D$query$layers$hardness[D$queryWarped$layers$queryLayerIndex])
})

test_that("resampled, uneven, bottom.up", {
  E <- dtwSP(SPpairs$C_day3, SPpairs$C_day1, resamplingRate = 0.5, rescale2refHS = FALSE,
             checkGlobalAlignment = FALSE, bottom.up = TRUE, top.down = FALSE, keep.internals = TRUE,
             dims = c("hardness", "gtype"), weights = c(0.2, 0.8),
             window.size = 0.3)
  expect_gte(simSP(E$reference, E$queryWarped), 0)
  expect_equal(dim(E$localCostMatrix), c(289, 221))
  expect_equal(E$localCostMatrix[289, 221], 0)
  expect_equal(E$imin, 186)
  expect_gt(E$queryWarped$hs, E$reference$hs)
  expect_equal(E$queryWarped$layers$hardness, E$query$layers$hardness[E$queryWarped$layers$queryLayerIndex])
})

test_that("resampled, uneven, top.down, unobservedBasalLayer, warp:jminTopDown", {
  G <- dtwSP(SPgroup[[5]], SPpairs$E_unobservedBase_modeled, resamplingRate = 0.5, rescale2refHS = FALSE,
             checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
             dims = c("hardness", "gtype"), weights = c(0.2, 0.8), window.size = 0.3)
  expect_match(G$openEndType_verbose, "matched_subsequence_of_reference")
  expect_match(G$openEndType, "jmin")
  expect_equal(hasUnobservedBasalLayer(G$queryWarped), TRUE)

  Grev <- dtwSP(SPpairs$E_unobservedBase_modeled, SPgroup[[5]], resamplingRate = 0.5, rescale2refHS = FALSE,
                checkGlobalAlignment = FALSE, bottom.up = FALSE, top.down = TRUE, keep.internals = TRUE,
                dims = c("hardness", "gtype"), weights = c(0.2, 0.8), window.size = 0.3)
  expect_match(Grev$openEndType_verbose, "matched_subsequence_of_query")
  expect_match(Grev$openEndType, "jmin")
  expect_equal(hasUnobservedBasalLayer(Grev$queryWarped), FALSE)
})


