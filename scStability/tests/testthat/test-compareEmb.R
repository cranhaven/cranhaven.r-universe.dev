testthat::skip_if_not_installed("scRNAseq", minimum_version = NULL)
testthat::skip_if_not_installed("SummarizedExperiment", minimum_version = NULL)

test_that("All embeddings are compared pairwise", {
  # now the Bioc packages are guaranteed available
  sce <- scRNAseq::ZeiselBrainData()
  counts_matrix <- SummarizedExperiment::assay(sce, "counts")
  counts_matrix <- as.matrix(counts_matrix[1:500, 1:50])

  emb_list <- scStability::createEmb(counts_matrix, n_runs = 5, n_cores = 5)
  result   <- scStability::compareEmb(emb_list)

  expect_equal(dim(result$all_pairwise_correlations), c(5, 5))
})


