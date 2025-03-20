skip_on_cran()
test_that("extractRegion errors when coverage files aren't saved locally", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  SampleTileMatrix@metadata$Directory <- "../../../HemeTutorial/idontexist"
  cellPopulation <- "C3"
  expect_error(
    MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = "ALL",
      region = "chr1:18137866-38139912",
      numCores = 30,
      sampleSpecific = FALSE
    ),
    "does not exist"
  )
})

skip_on_cran()
if (!dir.exists("../../../HemeTutorial/MOCHA")) {
  skip("HemeTutorial data not found at top level of package code")
}
test_that("extractRegion works on a 3 sample test dataset", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  # Since there are only cells for two samples in population "C3",
  # remove the empty sample.
  #
  # $CellCounts
  # cellTypeLabelList
  #                     C2  C3
  # scATAC_BMMC_R1      379  22
  # scATAC_CD34_BMMC_R1   0 302
  # scATAC_PBMC_R1        0   0
  #
  SampleTileMatrix <- SampleTileMatrix[
    , SampleTileMatrix$Sample %in% c("scATAC_BMMC_R1", "scATAC_CD34_BMMC_R1")
  ]

  SampleTileMatrix@metadata$Directory <- "../../../HemeTutorial/MOCHA"
  cellPopulation <- "C3"
  capture.output(
    countSE <- MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = cellPopulation,
      region = "chr1:18137866-38139912",
      numCores = 10,
      sampleSpecific = FALSE
    )
  )
  expect_snapshot_output(
    countSE
  )

  # Without binning
  capture.output(
    noBinningCountSE <- MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = cellPopulation,
      region = "chr1:18137866-18138866",
      numCores = 10,
      sampleSpecific = FALSE
    )
  )
  expect_snapshot_output(
    noBinningCountSE
  )
})

skip_on_cran()
test_that("extractRegion works on a 3 sample test dataset with sampleSpecific=TRUE", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  # Since there are only cells for two samples in population "C3",
  # remove the empty sample.
  #
  # $CellCounts
  # cellTypeLabelList
  #                     C2  C3
  # scATAC_BMMC_R1      379  22
  # scATAC_CD34_BMMC_R1   0 302
  # scATAC_PBMC_R1        0   0
  #
  SampleTileMatrix <- SampleTileMatrix[
    , SampleTileMatrix$Sample %in% c("scATAC_BMMC_R1", "scATAC_CD34_BMMC_R1")
  ]

  SampleTileMatrix@metadata$Directory <- "../../../HemeTutorial/MOCHA"
  cellPopulation <- "C3"
  capture.output(
    countSE <- MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = cellPopulation,
      region = "chr1:18137866-38139912",
      numCores = 10,
      sampleSpecific = TRUE
    )
  )
  expect_snapshot_output(
    countSE
  )

  # Without binning
  capture.output(
    noBinningCountSE <- MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = cellPopulation,
      region = "chr1:18137866-18138866",
      numCores = 10,
      sampleSpecific = TRUE
    )
  )
  expect_snapshot_output(
    noBinningCountSE
  )
})

skip_on_cran()
test_that("extractRegion errors when there is no fragment coverage for a cell population", {
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = "all",
      threshold = 0
    )
  )

  SampleTileMatrix@metadata$Directory <- "../../../HemeTutorial/MOCHA"
  cellPopulation <- "C3"
  expect_error(
    countSE <- MOCHA::extractRegion(
      SampleTileObj = SampleTileMatrix,
      cellPopulations = cellPopulation,
      region = "chr1:18137866-38139912",
      numCores = 30,
      sampleSpecific = FALSE
    ),
    "There is no fragment coverage for cell population"
  )
})
