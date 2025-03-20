# Test validRegionString
test_that(
  "validRegionString works on edge cases",
  {
    expect_true(MOCHA:::validRegionString("chr1:1-123412"))
    expect_true(MOCHA:::validRegionString("chr20:12300-12400"))
    expect_true(MOCHA:::validRegionString("4:145-146"))
    expect_false(MOCHA:::validRegionString("chr20_12300-12400"))
    expect_false(MOCHA:::validRegionString("chr20:12300-12200"))
  }
)

skip_on_cran()
# Working dir during tests is under structure:
# parent_dir/MOCHA/tests/testthat/. Assumes PBMCSmall is under 'parent_dir'
ArchRProjDir <- "../../../PBMCSmall"
if (dir.exists(ArchRProjDir) &&
    require("purrr", quietly = TRUE)) {
  # Load ArchR project
  capture.output(testProj <- ArchR::loadArchRProject(ArchRProjDir), type = "message")
  cellPopLabel <- "Clusters" # Column with cell population groups

  # Test getPopFrags with full genome and all normalization methods
  # Includes snapshot test for output
  NormMethod <- "nFrags"
  sampleSpecific <- TRUE
  test_that(
    stringr::str_interp(
      "getPopFrags works for NormMethod='${NormMethod}', sampleSpecific=${sampleSpecific}"
    ),
    {
      testthat::local_edition(3)
      capture.output(
        popFrags <- MOCHA::getPopFrags(testProj, cellPopLabel = cellPopLabel, numCores = 1),
        type = "message"
      )

      # Test population+sample names are as expected
      populations <- base::sort(unique(getCellColData(testProj)[[cellPopLabel]]))

      samples <- unique(testProj$Sample)
      combinations <- purrr::cross2(populations, samples) %>% purrr::map(purrr::lift(paste))
      expected_names <- gsub(" ", "#", combinations)

      sampleNames <- names(popFrags)
      actual_names <- base::sort(gsub("__.*", "", sampleNames))

      expect_equal(actual_names, expected_names)

      # Remove population+sample names and check that the content is equal
      names(popFrags) <- NULL

      snapshotVariant <- stringr::str_interp("${NormMethod}")
      if (sampleSpecific) {
        snapshotVariant <- paste(snapshotVariant, "sampleSpecific", sep = "_")
      }
      expect_snapshot_output(
        methods::as(popFrags, "list"),
        variant = snapshotVariant
      )
    }
  )


  # Test getPopFrags with specific region
  #   test_that(
  #     "getPopFrags works with a specific region when NormMethod='Raw'",
  #     {
  #       capture.output(
  #         popFrags <- MOCHA::getPopFrags(
  #           testProj,
  #           cellPopLabel = cellPopLabel,
  #           numCores = 1,
  #           region = "chr2:1-187350807",
  #           NormMethod = "Raw",
  #           sampleSpecific = FALSE
  #         ),
  #         type = "message"
  #       )

  #       # Test population names are as expected
  #       actual_names <- base::sort(gsub("__.*", "", names(popFrags)))
  #       expected_names <- base::sort(unique(getCellColData(testProj)[[cellPopLabel]]))
  #       expect_equal(actual_names, expected_names)
  #     }
  #   )

  # # Test getPopFrags with the wrong NormMethod when specifying a region
  # for (NormMethod in c("nFrags", "nCells", "Median", NULL)) {
  #   test_that(
  #     stringr::str_interp("getPopFrags throws an error if the wrong NormMethod (${NormMethod}) is set when asking for a specific region"),
  #     {
  #       expect_error(
  #         capture.output(popFrags <- MOCHA::getPopFrags(
  #           testProj,
  #           cellPopLabel = cellPopLabel,
  #           numCores = 1,
  #           region = "chr2:1-187350807",
  #           NormMethod = NormMethod,
  #           sampleSpecific = FALSE
  #         ), type = "message"),
  #         "Wrong NormMethod"
  #       )
  #     }
  #   )
  # }

  # Test getPopFrags with an incorrectly formatted region string
  # test_that(
  #   stringr::str_interp("getPopFrags throws an error for an incorrectly formatted region string"),
  #   {
  #     expect_error(
  #       capture.output(
  #         popFrags <- MOCHA::getPopFrags(
  #           testProj,
  #           cellPopLabel = cellPopLabel,
  #           numCores = 1,
  #           region = "chr2_1-187350807"
  #         ),
  #         type = "message"
  #       ),
  #       "Invalid region input."
  #     )
  #     # A character vector or region strings will work, but a list will not.
  #     expect_error(
  #       capture.output(
  #         popFrags <- MOCHA::getPopFrags(
  #           testProj,
  #           cellPopLabel = cellPopLabel,
  #           numCores = 1,
  #           region = list("chr1:1-2", "chr2:1-2")
  #         ),
  #         type = "message"
  #       ),
  #       "Invalid region input."
  #     )
  #   }
  # )


  # Test getPopFrags with a cellPopLabel not in cellColData
  test_that(
    stringr::str_interp("getPopFrags throws an error for a cellPopLabel not in cellColData"),
    {
      expect_error(
        capture.output(
          popFrags <- MOCHA::getPopFrags(
            testProj,
            cellPopLabel = "IDoNotExist",
            numCores = 1
          ),
          type = "message"
        ),
        "does not exist in the cellColData of your ArchRProj"
      )
    }
  )

  # Test getPopFrags with nonexistent cell subsets
  test_that(
    stringr::str_interp("getPopFrags throws an error for missing cellSubsets"),
    {
      expect_error(
        capture.output(
          popFrags <- MOCHA::getPopFrags(
            testProj,
            cellPopLabel = "Clusters",
            cellSubsets = c("C1", "IDoNotExist", "C3", "C5"),
            numCores = 1
          ),
          type = "message"
        ),
        "cellSubsets with NA cell counts: IDoNotExist"
      )
    }
  )

  test_that(
    "getPopFrags warns when cellPopLabels have protected delimiter characters",
    {
      newClusters <- c("01__HSC", "06_CLP.1", "10#cDC", "12__CD14.Mono.2", "13_CD16.Mono")
      clustersCol <- testProj$Clusters
      oldClusters <- unique(clustersCol)

      for (i in seq_along(newClusters)) {
        clustersCol[clustersCol == oldClusters[i]] <- newClusters[i]
      }
      testProj$newClusters <- clustersCol

      expect_warning(
        capture.output(
          popFrags <- MOCHA::getPopFrags(
            testProj,
            cellPopLabel = "newClusters",
            cellSubsets = "all",
            numCores = 1
          ),
          type = "message"
        ),
        "The following cell population labels contain protected delimiter"
      )
    }
  )

  test_that(
    "getPopFrags pools samples by cell population with poolSamples=TRUE",
    {
      capture.output(
        popFrags <- MOCHA::getPopFrags(
          testProj,
          cellPopLabel = "Clusters",
          cellSubsets = "all",
          poolSamples = TRUE,
          numCores = 1
        ),
        type = "message"
      )
      expect_snapshot_output(
        methods::as(popFrags, "list")
      )
    }
  )

  test_that(
    "getPopFrags pools samples by a single cellSubset with poolSamples=TRUE",
    {
      capture.output(
        popFrags <- MOCHA::getPopFrags(
          testProj,
          cellPopLabel = "Clusters",
          cellSubsets = "C5",
          poolSamples = TRUE,
          numCores = 1
        ),
        type = "message"
      )
      expect_snapshot_output(
        methods::as(popFrags, "list")
      )
    }
  )

  test_that(
    "getPopFrags pools samples by a single cellSubset with poolSamples=FALSE",
    {
      capture.output(
        popFrags <- MOCHA::getPopFrags(
          testProj,
          cellPopLabel = "Clusters",
          cellSubsets = "C5",
          poolSamples = FALSE,
          numCores = 1
        ),
        type = "message"
      )
      expect_snapshot_output(
        methods::as(popFrags, "list")
      )
    }
  )
}
