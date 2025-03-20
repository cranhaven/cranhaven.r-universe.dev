skip_if_not_installed("chromVAR")
skip_if_not_installed("BSgenome.Hsapiens.UCSC.hg19")
if (requireNamespace("chromVAR", quietly = TRUE) & 
    requireNamespace("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE)) {
  test_that("combineSampleTileMatrix works on a 3-sample dataset", {
    cellPopulations <- c("C2", "C3")
    capture.output(
      STObj <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = cellPopulations,
        threshold = 0
      )
    )

    combinedObj <- MOCHA::combineSampleTileMatrix(STObj)

    expect_snapshot_output(
      combinedObj,
      variant = "3sample"
    )
  })

  test_that("combineSampleTileMatrix works on a 1-sample dataset", {
    cellPopulations <- c("C2", "C5")
    capture.output(
      STObj <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResults,
        cellPopulations = cellPopulations,
        threshold = 0
      )
    )

    combinedObj <- MOCHA::combineSampleTileMatrix(STObj)

    expect_snapshot_output(
      combinedObj,
      variant = "1sample"
    )
  })
}
