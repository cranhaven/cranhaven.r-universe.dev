skip_on_cran() # Motif annotations take a long time
if (requireNamespace("chromVAR", quietly = TRUE) &&
  requireNamespace("chromVARmotifs", quietly = TRUE) &&
  requireNamespace("motifmatchr", quietly = TRUE) &&
  requireNamespace("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE)
) {
  test_that("addMotifSet works on a 3 sample test dataset", {
    cellPopulations <- c("C2", "C3")
    capture.output(
      STM <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = cellPopulations,
        threshold = 0
      )
    )

    STM <- MOCHA::addMotifSet(
      STM,
      chromVARmotifs::human_pwms_v2,
      motifSetName = "CISBP"
    )

    expect_true("CISBP" %in% names(metadata(STM)))
    expect_equal(class(metadata(STM)$CISBP)[1], "CompressedGRangesList")
  })
}
