test_that("We can subset a tileResults object by celltypes", {
  capture.output(
    obj <- MOCHA::subsetMOCHAObject(
      MOCHA:::testTileResultsMultisample,
      subsetBy = "celltypes",
      groupList = c("C3"),
      removeNA = TRUE,
      subsetPeaks = FALSE,
      verbose = FALSE
    )
  )

  expect_snapshot_output(
    obj,
    variant = "tileResults"
  )
  expect_snapshot_output(
    obj@metadata$summarizedData,
    variant = "tileResults"
  )
})


test_that("We can subset a tileResults object by Sample grouping", {
  skip_on_cran() # Unreproducible <RaggedExperiment>[,j] index out of bounds error
  capture.output(
    obj <- MOCHA::subsetMOCHAObject(
      MOCHA:::testTileResultsMultisample,
      subsetBy = "Sample",
      groupList = c("scATAC_CD34_BMMC_R1"),
      removeNA = TRUE,
      subsetPeaks = FALSE,
      verbose = FALSE
    )
  )

  expect_snapshot_output(
    obj,
    variant = "tileResults"
  )
  expect_snapshot_output(
    obj@metadata$summarizedData,
    variant = "tileResults"
  )
})

test_that("We can subset a sampleTileMatrix object by celltypes", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  capture.output(
    obj <- MOCHA::subsetMOCHAObject(
      SampleTileMatrix,
      subsetBy = "celltypes",
      groupList = c("C3"),
      removeNA = TRUE,
      subsetPeaks = FALSE,
      verbose = FALSE
    )
  )

  expect_snapshot_output(
    obj,
    variant = "sampleTileMatrix"
  )
  expect_snapshot_output(
    obj@metadata$summarizedData,
    variant = "sampleTileMatrix"
  )
})

test_that("We can subset a sampleTileMatrix object by Sample", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  capture.output(
    obj <- MOCHA::subsetMOCHAObject(
      SampleTileMatrix,
      subsetBy = "Sample",
      groupList = c("scATAC_CD34_BMMC_R1"),
      removeNA = TRUE,
      subsetPeaks = FALSE,
      verbose = FALSE
    )
  )

  expect_snapshot_output(
    obj,
    variant = "sampleTileMatrix"
  )
  expect_snapshot_output(
    obj@metadata$summarizedData,
    variant = "sampleTileMatrix"
  )
})

test_that("We can subset a sampleTileMatrix object - and peaks - by celltypes", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "All",
      threshold = 0
    )
  )

  capture.output(
    obj <- MOCHA::subsetMOCHAObject(
      SampleTileMatrix,
      subsetBy = "celltypes",
      groupList = c("C3"),
      removeNA = TRUE,
      subsetPeaks = TRUE,
      verbose = FALSE
    )
  )

  expect_snapshot_output(
    obj,
    variant = "sampleTileMatrix_peaks"
  )
  expect_snapshot_output(
    obj@metadata$summarizedData,
    variant = "sampleTileMatrix_peaks"
  )
})
