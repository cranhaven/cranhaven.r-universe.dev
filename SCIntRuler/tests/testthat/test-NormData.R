test_that("NormData", {
  data("sim_data_sce")
  sim_data <- SCEtoSeurat(sim_data_sce)
  seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
  normCount <- NormData(seuratlist)

  expect_type(normCount, "list")

})
