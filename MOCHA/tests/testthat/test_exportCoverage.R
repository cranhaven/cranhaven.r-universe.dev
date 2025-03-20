skip_on_cran()
if (dir.exists(MOCHA:::testTileResultsMultisample@metadata$Directory)) {
  test_that("exportCoverage works on a 3 sample test dataset", {
    
    capture.output(
      SampleTileMatrix <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = "all",
        threshold = 0
      )
    )
    
    mytempdir <- tempdir()
    
    capture.output(
      suppressWarnings(
        MOCHA::exportCoverage(
          SampleTileMatrix,
          dir = mytempdir,
          type = TRUE, # Coverage
          cellPopulations = "ALL",
          groupColumn = NULL,
          subGroups = NULL,
          sampleSpecific = FALSE,
          saveFile = TRUE,
          numCores = 1,
          verbose = FALSE
        )
      )
    )
    
    capture.output(
      suppressWarnings(
        outGR <- MOCHA::exportCoverage(
          SampleTileMatrix,
          dir = mytempdir,
          type = TRUE, # Coverage
          cellPopulations = "ALL",
          groupColumn = NULL,
          subGroups = NULL,
          sampleSpecific = FALSE,
          saveFile = FALSE,
          numCores = 1,
          verbose = FALSE
        )
      )
    )
    
    expect_equal(names(outGR), names(MOCHA:::testTileResultsMultisample))
  })
}

