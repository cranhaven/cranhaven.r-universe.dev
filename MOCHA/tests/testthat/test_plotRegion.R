skip_on_cran()
ArchRProjDir <- "../../../FullCovid"
if (
  require("TxDb.Hsapiens.UCSC.hg38.refGene", quietly = TRUE) &&
    require("org.Hs.eg.db", quietly = TRUE) &&
    require("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE) &&
    dir.exists(ArchRProjDir) &&
    require("ArchR", quietly = TRUE)
) {
  oldw <- getOption("warn")
  options(warn = -1)
  test_that("We can plot ", {
    ArchRProjDir <- "../../../FullCovid"
    # If running line-by-line:
    # ArchRProj <- ArchR::loadArchRProject("/home/jupyter/FullCovid")
    ArchRProj <- ArchR::loadArchRProject(ArchRProjDir)
    metadata <- data.table::as.data.table(ArchR::getCellColData(ArchRProj))
    studySignal <- median(metadata$nFrags)

    expect_equal(studySignal, 3628)

    # Get metadata information at the sample level
    lookup_table <- unique(
      metadata[, c(
        "Sample",
        "COVID_status",
        "Visit",
        "days_since_symptoms"
      ),
      with = FALSE
      ]
    )

    # Subset to visit 1 and extract samples
    samplesToKeep <- lookup_table$Sample[
      lookup_table$Visit == "FH3 COVID-19 Visit 1" &
        lookup_table$days_since_symptoms <= 15 |
        is.na(lookup_table$days_since_symptoms)
    ]

    # subset ArchR Project
    idxSample <- BiocGenerics::which(ArchRProj$Sample %in% samplesToKeep)
    cellsSample <- ArchRProj$cellNames[idxSample]
    ArchRProj <- ArchRProj[cellsSample, ]
    
    mytempdir <- tempdir()

    ###########################################################
    # 2. Call open tiles (main peak calling step)
    #    and get sample-tile matrices
    #    for all specified cell populations
    ###########################################################

    capture.output(tileResults <- MOCHA::callOpenTiles(
      ArchRProj,
      cellPopLabel = "CellSubsets",
      cellPopulations = "CD16 Mono",
      TxDb = "TxDb.Hsapiens.UCSC.hg38.refGene",
      Org = "org.Hs.eg.db",
      numCores = 50,
      studySignal = studySignal, # should be 3628
      outDir = mytempdir,
      verbose = TRUE
    ))

    ###########################################################
    # 3. Get consensus sample-tile matrices
    #    for all cell populations.
    #    These matrices are organized by cell population
    #    RangedSummarizedExperiment object and are the
    #    primary input to downstream analyses.
    ###########################################################

    capture.output(SampleTileMatrices <- MOCHA::getSampleTileMatrix(
      tileResults,
      cellPopulations = "CD16 Mono",
      groupColumn = "COVID_status",
      threshold = 0.2,
      verbose = FALSE
    ))

    ###########################################################
    # 4. Get differential accessibility for specific
    #    cell populations. Here we are comparing MAIT
    #    cells between samples where our groupColumn
    #    "COVID_status" is Positive (our foreground)
    #    to Negative samples (our background).
    ###########################################################

    capture.output(
      differentials <- MOCHA::getDifferentialAccessibleTiles(
        SampleTileObj = SampleTileMatrices,
        cellPopulation = "CD16 Mono",
        groupColumn = "COVID_status",
        foreground = "Positive",
        background = "Negative",
        outputGRanges = TRUE,
        numCores = 40
      )
    )
    cd16_differentials <- plyranges::filter(differentials, FDR <= 0.2)

    ###########################################################
    # 5. Extract a region of interest (usually a differential
    #    tile) for plotting
    ###########################################################
    region <- "chr4:122610000-122625000"
    capture.output(
      countSE <- extractRegion(
        SampleTileObj = SampleTileMatrices,
        region = region,
        cellPopulations = "CD16 Mono",
        groupColumn = "COVID_status",
        subGroups = NULL,
        sampleSpecific = FALSE,
        approxLimit = 1e+05,
        binSize = 250,
        numCores = 10,
        verbose = TRUE
      )
    )
    
    ###########################################################
    # 6. plot region
    ###########################################################
    pdf(file.path(mytempdir, "testplotregion.pdf"))
    MOCHA::plotRegion(countSE = countSE)
    dev.off()
    
    if (requireNamespace("RMariaDB", quietly = TRUE)) {
      pdf(file.path(mytempdir, "testplotregionwhichgene.pdf"))
      MOCHA::plotRegion(countSE = countSE,
                        whichGene = "IL21")
      dev.off()
    }
    # Remove mytempdir in case not generated with tempdir()
    unlink(mytempdir, recursive=TRUE)
    
  })
  options(warn = oldw)
}
