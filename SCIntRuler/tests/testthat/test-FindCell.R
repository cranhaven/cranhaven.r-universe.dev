test_that("FindCell", {
  data("sim_data_sce")
  data("sim_result")
  sim_data <- SCEtoSeurat(sim_data_sce)
  seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
  fullcluster <- sim_result[[1]]
  distmat <- sim_result[[3]]
  output <- FindCell(sim_data,seuratlist,fullcluster, distmat,firstn = 15)

  expect_type(output, "list")
})
