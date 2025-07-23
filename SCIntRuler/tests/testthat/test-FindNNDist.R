test_that("FindNNDist", {
  data(sim_result)
  meaningn <- 20

  distres <- FindNNDist(sim_result[[1]], sim_result[[2]], meaningn = meaningn)
  expect_type(distres, "list")
})
