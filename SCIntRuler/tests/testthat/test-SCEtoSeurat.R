test_that("SCEtoSeurat", {
  data("sim_data_sce")

  seurat_obj <- SCEtoSeurat(sim_data_sce)
  # expect_true(inherits(seurat_obj, "Seurat"))
  expect_equal(as.matrix(SummarizedExperiment::assay(sim_data_sce, "counts")),
               as.matrix(Seurat::GetAssayData(seurat_obj, slot = "counts")))

})
