test_that("The function prcomp() works for QTS samples", {
  res_pca <- prcomp(vespa64$igp[1:16])
  expect_snapshot(res_pca)
})

test_that("Visualization code for PCA work", {
  res_pca <- prcomp(vespa64$igp[1:16])
  p <- ggplot2::autoplot(res_pca, what = "PC1")
  expect_equal(dim(p$data), c(1212, 4))
  p <- ggplot2::autoplot(res_pca, what = "scores")
  expect_equal(dim(p$data), c(16, 2))
  p <- ggplot2::autoplot(res_pca, what = "variance")
  expect_equal(dim(p$data), c(5, 2))
})

test_that("Visualization functions for PCA work", {
  skip_if_not_installed("vdiffr")
  skip_on_covr()
  skip_on_ci()
  res_pca <- prcomp(vespa64$igp[1:16])
  vdiffr::expect_doppelganger(
    title = "PC plot",
    fig = plot(res_pca, what = "PC1")
  )
  vdiffr::expect_doppelganger(
    title = "Score plot",
    fig = plot(res_pca, what = "scores")
  )
  p <- ggplot2::autoplot(res_pca, what = "scores")
  vdiffr::expect_doppelganger(
    title = "Colored score plot",
    fig = p + ggplot2::geom_point(ggplot2::aes(color = vespa64$V[1:16]))
  )
  vdiffr::expect_doppelganger(
    title = "Screeplot",
    fig = plot(res_pca, what = "variance")
  )
})
