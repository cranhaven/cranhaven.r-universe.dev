skip_on_cran()
if (requireNamespace("TxDb.Hsapiens.UCSC.hg38.refGene", quietly = TRUE)) {
  test_that("getCoverage works on a 1 sample test dataset", {
    capture.output(
      covFiles <- MOCHA::getCoverage(
        popFrags = MOCHA::exampleFragments,
        normFactor = c(1, 1),
        filterEmpty = FALSE,
        cl = 1, TxDb = TxDb.Hsapiens.UCSC.hg38.refGene
      )
    )
    expect_snapshot(covFiles)
  })

  test_that("getCoverage works on a 1 sample test dataset with filterEmpty=TRUE", {
    capture.output(
      covFiles <- MOCHA::getCoverage(
        popFrags = MOCHA::exampleFragments,
        normFactor = c(1, 1),
        filterEmpty = TRUE,
        cl = 1, TxDb = TxDb.Hsapiens.UCSC.hg38.refGene
      )
    )
    expect_snapshot(covFiles)
  })

  test_that("getSpecificCoverage works on a 1 sample test dataset", {
    capture.output(
      covFiles <- MOCHA::getCoverage(
        popFrags = MOCHA::exampleFragments,
        normFactor = c(1, 1),
        filterEmpty = TRUE,
        cl = 1, TxDb = TxDb.Hsapiens.UCSC.hg38.refGene
      )
    )
    regions <- StringsToGRanges(c("chr1:565291-569412", "chr2:243031253-243034339"))
    covFiles <- covFiles$Accessibility
    seqinfo(regions) <- GenomicRanges::seqinfo(covFiles[[1]])
    counts <- MOCHA:::getSpecificCoverage(
      covFiles,
      regions,
      numCores = 1
    )
    expect_snapshot(counts)
  })
}
