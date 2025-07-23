test_that("CalcuSCIR", {
  data(sim_result)
  data(sim_data_sce)
  sim_data <- SCEtoSeurat(sim_data_sce)
  seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
  SCIR <- CalcuSCIR(sim_result[[1]], seuratlist, sim_result[[4]])
  expect_type(SCIR, "double")


})
