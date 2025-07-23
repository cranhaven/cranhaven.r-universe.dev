test_that("FindNNDistC", {
  data(sim_result)
  meaningn <- 20

  distres <- FindNNDistC(sim_result[[1]], sim_result[[2]], meaningn = meaningn)
  expect_type(distres, "list")
})
