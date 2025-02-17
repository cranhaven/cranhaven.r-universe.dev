test_that("Function DTW() works", {
  expect_snapshot(DTW(vespa64$igp[[1]], vespa64$igp[[2]]))
})
