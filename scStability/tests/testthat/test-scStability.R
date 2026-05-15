testthat::skip_if_not_installed("scRNAseq", minimum_version = NULL)
testthat::skip_if_not_installed("SummarizedExperiment", minimum_version = NULL)

test_that("Check final embeddings and clusters", {
  sce <- scRNAseq::ZeiselBrainData()
  counts_matrix <- SummarizedExperiment::assay(sce, "counts")
  rownames(counts_matrix) <- gsub("_", "-", rownames(counts_matrix))
  seurat_obj <- Seurat::CreateSeuratObject(counts_matrix)
  cell_names <- colnames(seurat_obj@assays$RNA)[1:150]
  seurat_obj <- subset(seurat_obj, cells = cell_names)
  seurat_obj <- Seurat::NormalizeData(seurat_obj)
  seurat_obj <- Seurat::FindVariableFeatures(seurat_obj)
  seurat_obj <- Seurat::ScaleData(seurat_obj)
  seurat_obj <- Seurat::RunPCA(seurat_obj)
  result <- scStability::scStability(seurat_obj, n_runs = 5, n_cores = 5)
  expect_equal(dim(result$mean_emb), c(150, 2))
  expect_equal(length(result$mean_clust), 150)
})
