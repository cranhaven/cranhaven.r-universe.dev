test_that("Function dbscan.default() works", {
  iris <- as.matrix(iris[, 1:4])
  out <- dbscan(iris, eps = .7, minPts = 5)
  expect_equal(length(out), 5)
})

test_that("Function dbscan.qts_sample() works", {
  out <- dbscan(vespa64$igp[1:10])
  expect_equal(out$best_clustering$n_clusters, 1)
})
