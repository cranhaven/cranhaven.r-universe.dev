test_that("exportDifferentials works on a 3 sample test dataset", {
  
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )
  
  capture.output(
    suppressWarnings(
      differentials <- MOCHA::getDifferentialAccessibleTiles(
        SampleTileMatrix,
        cellPopulation = "C3",
        groupColumn = "Sample",
        foreground = "scATAC_BMMC_R1",
        background = "scATAC_CD34_BMMC_R1",
        outputGRanges = TRUE,
        numCores = 1,
        verbose = FALSE
      )
    )
  )
  
  mytempdir <- tempdir()
  
  out <- MOCHA::exportDifferentials(
    SampleTileObject = SampleTileMatrix,
    DifferentialsGRList = list(differentials),
    outDir = mytempdir,
    verbose = TRUE
  )
  
  expect_true(all(do.call(file.exists, out)))
})