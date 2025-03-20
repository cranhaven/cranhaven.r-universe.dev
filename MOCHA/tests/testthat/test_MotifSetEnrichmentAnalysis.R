skip_on_cran()
motifEnrichmentTestDataFP <- "/home/jupyter/MOCHA/input_motifenrichment.csv"
expectedResultsFP <- "/home/jupyter/MOCHA/results_MSEA.csv"
if (file.exists(motifEnrichmentTestDataFP) &&
  file.exists(expectedResultsFP)
) {
  tryCatch(
    ligandTFMatrix <- readRDS(
      url("https://zenodo.org/record/3260758/files/ligand_tf_matrix.rds")
    ),
    error = function(e) {
      message(e)
      stop("ligandTFMatrix Zenodo link is broken")
    }
  )

  motifEnrichmentDF <- read.csv(motifEnrichmentTestDataFP)
  expectedResults <- read.csv(expectedResultsFP)

  filteredligandTFMatrix <- ligandTFMatrix[
    rownames(ligandTFMatrix) %in% unique(motifEnrichmentDF$TranscriptionFactor),
  ]

  test_that("MotifSetEnrichmentAnalysis works on three different orderings of input ligands", {
    ligandsv1 <- expectedResults$ligand
    ligandsv2 <- colnames(filteredligandTFMatrix)[colSums(filteredligandTFMatrix) > 0]
    ligandsv3 <- base::sort(ligandsv2)

    expect_true(identical(base::sort(ligandsv1), ligandsv3))

    for (ligands in list(ligandsv1, ligandsv2, ligandsv3)) {
      results <- MOCHA::MotifSetEnrichmentAnalysis(
        filteredligandTFMatrix,
        motifEnrichmentDF,
        motifColumn = "TranscriptionFactor",
        ligands = ligands,
        statColumn = "mlog10Padj",
        statThreshold = 2,
        verbose = FALSE
      )

      results <- dplyr::arrange(results, ligand)
      expectedResults <- dplyr::arrange(expectedResults, ligand)

      expect_snapshot(results)

      expect_true(identical(expectedResults$ligands, results$ligands))
      expect_true(all(expectedResults$ligands == results$ligands))
      expect_true(identical(round(expectedResults$adjp_val, 4), round(results$adjp_val, 4)))
      expect_true(identical(round(expectedResults$PercentSigTF, 4), round(results$PercentSigTF, 4)))
      expect_true(identical(round(expectedResults$PercInNicheNet, 4), round(results$PercInNicheNet, 4)))
      expect_true(identical(round(expectedResults$p_val, 4), round(results$p_val, 4)))
    }
  })

  test_that("MotifSetEnrichmentAnalysis correctly filters an unfiltered NicheNet matrix", {
    results <- MOCHA::MotifSetEnrichmentAnalysis(
      ligandTFMatrix,
      motifEnrichmentDF,
      motifColumn = "TranscriptionFactor",
      ligands = expectedResults$ligand,
      statColumn = "mlog10Padj",
      statThreshold = 2,
      verbose = FALSE
    )

    results <- dplyr::arrange(results, ligand)
    expectedResults <- dplyr::arrange(expectedResults, ligand)

    expect_snapshot(results)

    expect_true(identical(expectedResults$ligands, results$ligands))
    expect_true(all(expectedResults$ligands == results$ligands))
    expect_true(identical(round(expectedResults$adjp_val, 4), round(results$adjp_val, 4)))
    expect_true(identical(round(expectedResults$PercentSigTF, 4), round(results$PercentSigTF, 4)))
    expect_true(identical(round(expectedResults$PercInNicheNet, 4), round(results$PercInNicheNet, 4)))
    expect_true(identical(round(expectedResults$p_val, 4), round(results$p_val, 4)))
  })
}
