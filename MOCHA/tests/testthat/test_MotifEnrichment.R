skip_on_cran()
test_that("MotifEnrichment works with mock data", {
  testPeaks <- MOCHA::differentialsToGRanges(
    MOCHA:::testPeaks,
    tileColumn = "tileID"
  )
  Group1 <- plyranges::filter(testPeaks, FDR < 0.1)
  Group2 <- plyranges::filter(testPeaks, FDR >= 0.1)

  FDR <- NULL
  enrichDAP_df <- MotifEnrichment(
    Group1 = Group1,
    Group2 = Group2,
    motifPosList = MOCHA:::testPosList
  )

  expect_snapshot_output(
    enrichDAP_df
  )

  expect_equal(nrow(enrichDAP_df), length(MOCHA:::testPosList))
})

skip_on_cran()
test_that("MotifEnrichment catches non-GRanges input", {
  Group1 <- dplyr::filter(MOCHA:::testPeaks, FDR < 0.1)
  Group2 <- dplyr::filter(MOCHA:::testPeaks, FDR >= 0.1)

  FDR <- NULL
  expect_error(
    MotifEnrichment(
      Group1 = Group1,
      Group2 = Group2,
      motifPosList = testPosList
    )
  )
})
