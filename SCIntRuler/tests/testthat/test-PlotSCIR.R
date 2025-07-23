test_that("PlotSCIR", {

  data(sim_result)
  data("sim_data_sce")
  sim_data <- SCEtoSeurat(sim_data_sce)
  seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
  plot <- PlotSCIR(sim_result[[1]], seuratlist, sim_result[[4]])

  expect_true(ggplot2::is.ggplot(plot))


})
