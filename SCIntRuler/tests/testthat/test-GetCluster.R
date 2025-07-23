test_that("GetCluster", {
  data("sim_data_sce")
  sim_data <- SCEtoSeurat(sim_data_sce)
  seuratlist <- Seurat::SplitObject(sim_data, split.by = "Study")
  fullcluster <- GetCluster(seuratlist)

  expect_type(fullcluster, "list")
})
