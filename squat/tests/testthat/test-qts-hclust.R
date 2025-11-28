test_that("Function hclust.default() works", {
  hc <- hclust(dist(USArrests), linkage_criterion = "average")
  expect_equal(length(hc), 7)
})

test_that("Function hclust.qts_sample() works", {
  out <- hclust(vespa64$igp[1:10], n_clusters = 2)
  expect_equal(out$best_clustering$n_clusters, 2)
})
