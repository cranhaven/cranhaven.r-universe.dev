skip_on_cran()
if (
  require("TxDb.Hsapiens.UCSC.hg38.refGene", quietly = TRUE) &&
  require("org.Hs.eg.db", quietly = TRUE) &&
  require("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE)
) {
  # Working dir during tests is under projects/MOCHA/tests/testthat/. Assumes
  # PBMCSmall is under 'projects'
  ArchRProjDir <- "../../../PBMCSmall"
  if (require("ArchR", quietly = TRUE) & dir.exists(ArchRProjDir)) {
    test_that("We can pack/unpack a MOCHA object", {
      capture.output(
        testProj <- ArchR::loadArchRProject(ArchRProjDir),
        type = "message"
      )

      TxDb <- "TxDb.Hsapiens.UCSC.hg38.refGene"
      OrgDb <- "org.Hs.eg.db"
      
      mytempdir <- tempdir()
      
      capture.output(
        tiles <- MOCHA::callOpenTiles(
          ATACFragments = testProj,
          TxDb = TxDb,
          OrgDb = OrgDb,
          cellPopLabel = "Clusters",
          cellPopulations = c("C2", "C5"),
          numCores = 1,
          outDir = mytempdir
        ),
        type = "message"
      )
      
      zipPath <- MOCHA::packMOCHA(tiles, zipfile = file.path(mytempdir, "testzip.zip"))
      expect_true(file.exists(zipPath))
      expect_error(
        MOCHA::packMOCHA(tiles, zipfile = file.path(mytempdir, "testzip.zap"))
      )
      
      if (requireNamespace("waldo")) {
        unpackedmochaObj <- MOCHA::unpackMOCHA(zipPath, mytempdir)
        diff <- waldo::compare(unpackedmochaObj, tiles)
        expect_length(diff, 1)
        expect_true(grepl('metadata$Directory', diff[1], fixed = TRUE))
      }
    })
  }
}
