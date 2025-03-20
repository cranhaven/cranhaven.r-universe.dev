if (require(chromVARmotifs)) { 
  test_that("exportDifferentials works on a 3 sample test dataset", {
    
    capture.output(
      SampleTileMatrix <- MOCHA::getSampleTileMatrix(
        MOCHA:::testTileResultsMultisample,
        cellPopulations = "all",
        threshold = 0
      )
    )
    
    mytempdir <- tempdir()
    data("human_pwms_v2")
    
    motifsGRanges <- MOCHA::addMotifSet(
      SampleTileMatrix,
      motifPWMs = human_pwms_v2,
      returnSTM=FALSE)
    
    out <- MOCHA::exportMotifs(
      SampleTileObj = SampleTileMatrix,
      unlist(motifsGRanges),
      motifSetName = "CISBP",
      filterByOpenTiles = FALSE,
      outDir = tempdir(),
      verbose = TRUE
    )
    
    expect_true(all(do.call(file.exists, out)))
  })
}