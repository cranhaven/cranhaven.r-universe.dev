test_that("exportDifferentials works on a 3 sample test dataset", {
  
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )
  
  mytempdir <- tempdir()
  
  out <- MOCHA::exportOpenTiles(
    SampleTileObj = SampleTileMatrix,
    cellPopulation = "C3",
    outDir = mytempdir,
    verbose = FALSE
  )
  
  expect_true(all(do.call(file.exists, out)))
})