test_that("Functions related to the QTS class work", {
  qts1 <- vespa64$igp[[1]]
  expect_true(is_qts(qts1))
  qts2 <- as_qts(qts1)
  expect_true(is_qts(qts2))
  expect_equal(qts1, qts2)
  qts3 <- qts1
  class(qts3) <- class(qts3)[-1]
  expect_false(is_qts(qts3))
  qts3 <- as_qts(qts1)
  expect_equal(qts1, qts3)
})

test_that("Function centring() works (standardize = FALSE, keep_summary_stats = FALSE)", {
  expect_snapshot(centring(
    x = vespa64$igp[[1]],
    standardize = FALSE,
    keep_summary_stats = FALSE
  ))
})

test_that("Function centring() works (standardize = TRUE, keep_summary_stats = FALSE)", {
  expect_snapshot(centring(
    x = vespa64$igp[[1]],
    standardize = TRUE,
    keep_summary_stats = FALSE
  ))
})

test_that("Function centring() works (standardize = FALSE, keep_summary_stats = TRUE)", {
  expect_snapshot(centring(
    x = vespa64$igp[[1]],
    standardize = FALSE,
    keep_summary_stats = TRUE
  ))
})

test_that("Visualization code for QTS work", {
  p <- ggplot2::autoplot(vespa64$igp[[1]])
  expect_equal(dim(p$data), c(404, 3))
  p <- ggplot2::autoplot(vespa64$igp[[1]], highlighted_points = c(10, 80))
  expect_equal(dim(p$data), c(404, 3))
})

test_that("Visualization functions for QTS work", {
  skip_if_not_installed("vdiffr")
  skip_on_covr()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "QTS plot",
    fig = plot(vespa64$igp[[1]])
  )
  vdiffr::expect_doppelganger(
    title = "QTS plot with change points",
    fig = plot(vespa64$igp[[1]], highlighted_points = c(10, 80))
  )
})
