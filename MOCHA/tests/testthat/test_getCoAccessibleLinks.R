test_that("FindCoAccessibleLinks works on a 1 sample test dataset", {
  cellPopulations <- c("C2", "C5")
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResults,
      cellPopulations = cellPopulations,
      threshold = 0
    )
  )

  cellPopulation <- "C2"
  regions <- MOCHA::StringsToGRanges(c(
    "chr1:101873000-101873499"
  ))
  links <- MOCHA::getCoAccessibleLinks(SampleTileMatrix,
    cellPopulation,
    regions,
    verbose = FALSE
  )

  expect_false(any((links$Peak1 == links$Peak2)))
  expect_snapshot_output(
    links,
    variant = "tiles"
  ) # change variant to "basic" to compare with previous co_accessibility.R
  expect_snapshot_output(
    nrow(links),
    variant = "nrows"
  )
})

test_that("FindCoAccessibleLinks works on a 3 sample test dataset", {
  cellPopulations <- c("C2", "C3")
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResultsMultisample,
      cellPopulations = cellPopulations,
      threshold = 0
    )
  )

  cellPopulation <- "C2"
  regions <- MOCHA::StringsToGRanges(c(
    "chr1:10009500-10009999"
  ))
  links <- MOCHA::getCoAccessibleLinks(SampleTileMatrix,
    cellPopulation,
    regions,
    verbose = FALSE
  )

  expect_false(any((links$Peak1 == links$Peak2)))
  expect_snapshot_output(
    links,
    variant = "tiles_3sample"
  ) # change variant to "basic" to compare with previous co_accessibility.R
  expect_snapshot_output(
    nrow(links),
    variant = "nrows_3sample"
  )
})

test_that("FindCoAccessibleLinks errors with regions that are not valid tiles", {
  cellPopulations <- c("C2")
  capture.output(
    SampleTileMatrix <- MOCHA::getSampleTileMatrix(
      MOCHA:::testTileResults,
      cellPopulations = cellPopulations,
      threshold = 0
    )
  )

  cellPopulation <- "C2"
  regions <- MOCHA::StringsToGRanges(c(
    "chr1:102368000-102368499", # does not exist
    "chr1:10003000-10003499", # valid
    "chr3:102368000-102368499" # does not exist
  ))
  expect_error(
    links <- MOCHA::getCoAccessibleLinks(SampleTileMatrix,
      cellPopulation,
      regions,
      verbose = FALSE
    ),
    "chr1:102368000-102368499, chr3:102368000-102368499"
  )
})
