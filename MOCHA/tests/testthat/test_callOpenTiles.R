# This test should only be used for local testing
# with TxDb.Hsapiens.UCSC.hg38.refGene and org.Hs.eg.db installed/
skip_on_cran()
if (
  require("TxDb.Hsapiens.UCSC.hg38.refGene", quietly = TRUE) &&
    require("org.Hs.eg.db", quietly = TRUE) &&
    require("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE) &&
  require("TxDb.Hsapiens.UCSC.hg19.knownGene", quietly = TRUE)
) {
  # Working dir during tests is under projects/MOCHA/tests/testthat/. Assumes
  # PBMCSmall is under 'projects'
  ArchRProjDir <- "../../../PBMCSmall"
  if (require("ArchR", quietly = TRUE) & dir.exists(ArchRProjDir)) {
    test_that("We can call peaks by sample from an ArchR project", {
      capture.output(
        testProj <- ArchR::loadArchRProject(ArchRProjDir),
        type = "message"
      )

      TxDb <- "TxDb.Hsapiens.UCSC.hg38.refGene"
      OrgDb <- "org.Hs.eg.db"
      capture.output(
        tiles <- MOCHA::callOpenTiles(
          ATACFragments = testProj,
          TxDb = TxDb,
          OrgDb = OrgDb,
          cellPopLabel = "Clusters",
          cellPopulations = c("C2", "C5"),
          numCores = 1,
          outDir = tempdir()
        ),
        type = "message"
      )

      expect_snapshot(
        tiles,
        variant = "ArchR"
      )

      tiles@metadata$Directory <- NULL # Directory uses tempdir()
      expect_snapshot(
        tiles@metadata,
        variant = "ArchR_metadata"
      )
    })
  }

  test_that("We can call peaks independent of ArchR", {
    TxDb <- "TxDb.Hsapiens.UCSC.hg38.refGene"
    OrgDb <- "org.Hs.eg.db"
    capture.output(
      tiles <- MOCHA::callOpenTiles(
        ATACFragments = MOCHA::exampleFragments,
        cellColData = MOCHA::exampleCellColData,
        blackList = MOCHA::exampleBlackList,
        genome = "hg19",
        TxDb = TxDb,
        OrgDb = OrgDb,
        outDir = tempdir(),
        cellPopLabel = "Clusters",
        cellPopulations = c("C2", "C5"),
        numCores = 1
      ),
      type = "message"
    )

    expect_snapshot(
      tiles,
      variant = "list"
    )
    expect_snapshot(
      assays(metadata(tiles)$summarizedData)[["CellCounts"]],
      variant = "CellCounts"
    )
    expect_snapshot(
      assays(metadata(tiles)$summarizedData)[["FragmentCounts"]],
      variant = "FragmentCounts"
    )

    tiles@metadata$Directory <- NULL # Directory uses tempdir()
    expect_snapshot(
      tiles@metadata,
      variant = "list_metadata"
    )
  })

  test_that("We throw a warning when a sample has less than 5 cells", {
    TxDb <- "TxDb.Hsapiens.UCSC.hg38.refGene"
    OrgDb <- "org.Hs.eg.db"
    sample1frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(760101:760110), end = c(760111:760120), names = head(letters, 10)),
      strand = "*",
      RG = c("c1", "c2", "c2", "c3", "c3", "c4", "c4", "c4", "c5", "c5")
    )
    sample2frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(760101:760110), end = c(760111:760120), names = head(letters, 10)),
      strand = "*",
      RG = c("c6", "c7", "c7", "c8", "c8", "c8", "c9", "c9", "c9", "c9")
    )
    tiny_fragments <- GenomicRanges::GRangesList(sample1frags, sample2frags)
    names(tiny_fragments) <- c("t_cd8_temra#sample1", "t_cd8_temra#sample2")

    tiny_cellColData <- data.frame(
      Sample = c(rep("sample1", 5), rep("sample2", 4)),
      cellPop = rep("t_cd8_temra", 9)
    )
    rownames(tiny_cellColData) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")

    expect_warning(tiles <- MOCHA::callOpenTiles(
      ATACFragments = tiny_fragments,
      cellColData = tiny_cellColData,
      blackList = MOCHA::exampleBlackList,
      genome = "hg19",
      TxDb = TxDb,
      OrgDb = OrgDb,
      outDir = tempdir(),
      cellPopLabel = "cellPop",
      cellPopulations = c("t_cd8_temra"),
      studySignal = 10, # manually provide or have nFrags col in cellColData
      numCores = 1, verbose = TRUE
    ))
  })

  test_that("We error when nFrags is absent from cellColData", {
    sample1frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(760101:760110), end = c(760111:760120), names = head(letters, 10)),
      strand = "*",
      RG = c("c1", "c2", "c2", "c3", "c3", "c4", "c4", "c4", "c5", "c5")
    )
    sample2frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(760101:760110), end = c(760111:760120), names = head(letters, 10)),
      strand = "*",
      RG = c("c6", "c7", "c7", "c8", "c8", "c8", "c9", "c9", "c9", "c9")
    )
    tiny_fragments <- GenomicRanges::GRangesList(sample1frags, sample2frags)
    names(tiny_fragments) <- c("t_cd8_temra#sample1", "t_cd8_temra#sample2")

    tiny_cellColData <- data.frame(
      Sample = c(rep("sample1", 5), rep("sample2", 4)),
      cellPop = rep("t_cd8_temra", 9)
    )
    rownames(tiny_cellColData) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")

    expect_error(tiles <- MOCHA::callOpenTiles(
      ATACFragments = tiny_fragments,
      cellColData = tiny_cellColData,
      blackList = MOCHA::exampleBlackList,
      genome = "hg19",
      TxDb = TxDb,
      OrgDb = OrgDb,
      outDir = tempdir(),
      cellPopLabel = "cellPop",
      cellPopulations = c("t_cd8_temra"),
      studySignal = NULL, # manually provide or have nFrags col in cellColData
      numCores = 1, verbose = FALSE
    ))
  })

  test_that("We error when our fragments are all blacklisted", {
    sample1frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(101:110), end = c(111:120), names = head(letters, 10)),
      strand = "*",
      RG = c("c1", "c2", "c2", "c3", "c3", "c4", "c4", "c4", "c5", "c5")
    )
    sample2frags <- GenomicRanges::GRanges(
      seqnames = Rle(c("chr1"), c(1)),
      ranges = IRanges(c(101:110), end = c(111:120), names = head(letters, 10)),
      strand = "*",
      RG = c("c6", "c7", "c7", "c8", "c8", "c8", "c9", "c9", "c9", "c9")
    )
    tiny_fragments <- GenomicRanges::GRangesList(sample1frags, sample2frags)
    names(tiny_fragments) <- c("t_cd8_temra#sample1", "t_cd8_temra#sample2")

    tiny_cellColData <- data.frame(
      Sample = c(rep("sample1", 5), rep("sample2", 4)),
      cellPop = rep("t_cd8_temra", 9)
    )
    rownames(tiny_cellColData) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9")

    expect_error(tiles <- MOCHA::callOpenTiles(
      ATACFragments = tiny_fragments,
      cellColData = tiny_cellColData,
      blackList = MOCHA::exampleBlackList,
      genome = "hg19",
      TxDb = TxDb,
      OrgDb = OrgDb,
      outDir = tempdir(),
      cellPopLabel = "cellPop",
      cellPopulations = c("t_cd8_temra"),
      studySignal = 10, # manually provide or have nFrags col in cellColData
      numCores = 1, verbose = TRUE
    ))
  })

  test_that("We error informatively when cellPopLabel is not in the metadata", {
    TxDb <- "TxDb.Hsapiens.UCSC.hg38.refGene"
    OrgDb <- "org.Hs.eg.db"
    expect_error(
      tiles <- MOCHA::callOpenTiles(
        ATACFragments = MOCHA::exampleFragments,
        cellColData = MOCHA::exampleCellColData,
        blackList = MOCHA::exampleBlackList,
        genome = "hg19",
        TxDb = TxDb,
        OrgDb = OrgDb,
        outDir = tempdir(),
        cellPopLabel = "INCORRECTCELLPOPLABEL",
        cellPopulations = c("C2", "C5"),
        numCores = 1
      ),
      regexp = "cellColData must contain column 'cellPopLabel'"
    )
  })
}
