skip_if_not_installed("BSgenome.Hsapiens.UCSC.hg19")
if (requireNamespace("irlba", quietly = TRUE) &&
  requireNamespace("Matrix", quietly = TRUE) &&
  requireNamespace("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE) &&
  requireNamespace("uwot", quietly = TRUE)) {
  test_that("bulkDimReduction works on a 3 sample test dataset", {
    cellPopulations <- c("C2", "C3")
    capture.output(
      SampleTileMatrix <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = cellPopulations,
        threshold = 0
      ),
      type = "message"
    )

    # Spoof data to remove NAs
    c2 <- SummarizedExperiment::assays(SampleTileMatrix)[[1]]
    set.seed(1)
    c2[, 2] <- stats::runif(dim(c2)[1], min = 1100, max = 8700)
    set.seed(2)
    c2[, 3] <- stats::runif(dim(c2)[1], min = 1100, max = 8700)
    SummarizedExperiment::assays(SampleTileMatrix)[[1]] <- c2

    # Spoof data to remove NAs
    c3 <- SummarizedExperiment::assays(SampleTileMatrix)[[2]]
    set.seed(3)
    c3[, 2] <- stats::runif(dim(c3)[1], min = 1100, max = 8700)
    set.seed(4)
    c3[, 3] <- stats::runif(dim(c3)[1], min = 1100, max = 8700)
    SummarizedExperiment::assays(SampleTileMatrix)[[2]] <- c3

    capture.output(
      LSIObj <- MOCHA::bulkDimReduction(SampleTileMatrix, cellType = "all", componentNumber = 2)
    )

    expect_snapshot(
      LSIObj
    )
  })

  test_that("bulkDimReduction errors NA columns", {
    cellPopulations <- c("C2", "C3")
    capture.output(
      SampleTileMatrix <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = cellPopulations,
        threshold = 0
      ),
      type = "message"
    )

    expect_error(
      LSIse <- MOCHA::bulkDimReduction(SampleTileMatrix, cellType = "all", componentNumber = 2),
      "starting vector near the null space"
    )
  })
}
