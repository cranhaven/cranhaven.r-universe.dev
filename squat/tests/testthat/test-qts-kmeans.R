test_that("Function kmeans.default() works", {
  out <- kmeans(USArrests, n_clusters = 2)
  expect_equal(length(out), 9)
})

test_that("Function kmeans.qts_sample() works", {
  withr::with_seed(1234, {
    out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
  })
  expect_equal(out$best_clustering$n_clusters, 2)
})

test_that("Visualization code for k-means work", {
  withr::with_seed(1234, {
    out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
  })
  p <- ggplot2::autoplot(out)
  expect_equal(dim(p$data), c(808, 6))
})

test_that("Visualization functions for PCA work", {
  skip_if_not_installed("vdiffr")
  skip_on_covr()
  skip_on_ci()
  withr::with_seed(1234, {
    out <- kmeans(vespa64$igp[1:10], n_clusters = 2)
  })
  vdiffr::expect_doppelganger(
    title = "K-means plot",
    fig = plot(out)
  )
})
