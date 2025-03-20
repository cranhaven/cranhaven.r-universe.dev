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
  test_that("We reproduce the COVID CD16 Monocyte analysis", {
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
      outDir = tempdir()
    ))

    # Snapshot test of summary()
    tileResultAssay <- RaggedExperiment::compactAssay(
      MultiAssayExperiment::experiments(tileResults)[[1]],
      i = "TotalIntensity"
    )
    expect_snapshot(summary(tileResultAssay))
    expect_snapshot(head(tileResultAssay))
    expect_snapshot(tail(tileResultAssay))

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

    # Snapshot test of summary()
    SampleTileMatrixAssay <- assays(SampleTileMatrices)[[1]]
    expect_snapshot(summary(SampleTileMatrixAssay))
    expect_snapshot(head(SampleTileMatrixAssay))
    expect_snapshot(tail(SampleTileMatrixAssay))

    capture.output(cd16matrix <- MOCHA::getCellPopMatrix(
      SampleTileObj = SampleTileMatrices,
      cellPopulation = "CD16 Mono",
      dropSamples = TRUE,
      NAtoZero = TRUE
    ))

    # Snapshot test of summary()
    expect_snapshot(summary(cd16matrix))
    expect_snapshot(head(cd16matrix))
    expect_snapshot(tail(cd16matrix))

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

    # GRanges snapshot includes head/tail of all values
    expect_snapshot(cd16_differentials)
    expect_equal(length(cd16_differentials), 6211)

    # Expected values from results of CD16 Monocyte analysis
    FDRSummary <- summary(cd16_differentials$FDR)
    expect_equal(round(FDRSummary[["Min."]], 11), 0.01887700776)
    expect_equal(round(FDRSummary[["Max."]], 7), 0.1983282)
    expect_equal(round(FDRSummary[["Mean"]], 7), 0.1192888)
    expect_equal(round(FDRSummary[["Median"]], 7), 0.1211614)

    IntensitySummary <- summary(cd16_differentials$Avg_Intensity_Case)
    expect_equal(round(IntensitySummary[["Min."]], 6), 7.522832)
    expect_equal(round(IntensitySummary[["Max."]], 5), 16.88269)
    expect_equal(round(IntensitySummary[["Mean"]], 5), 13.14367)
    expect_equal(round(IntensitySummary[["Median"]], 5), 13.21955)

    capture.output(
      unFilteredSummaries <- MOCHA::getDifferentialAccessibleTiles(
        SampleTileObj = SampleTileMatrices,
        cellPopulation = "CD16 Mono",
        groupColumn = "COVID_status",
        foreground = "Positive",
        background = "Negative",
        outputGRanges = FALSE,
        numCores = 40,
        signalThreshold = 0,
        minZeroDiff = 0
      )
    )
    expect_snapshot(unFilteredSummaries)
    expect_equal(dim(unFilteredSummaries)[1], 215649)
  })
  options(warn = oldw)
}
