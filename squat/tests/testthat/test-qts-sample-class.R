test_that("Functions related to the QTS class work", {
  expect_true(is_qts_sample(vespa64$igp))
  expect_true(is_qts_sample(as_qts_sample(vespa64$igp)))
  x <- vespa64$igp[1, simplify = TRUE]
  expect_true(is_qts(x))
  expect_equal(x, vespa64$igp[[1]])
  expect_snapshot(vespa64$igp[1])
})

test_that("The function append() works", {
  x <- append(vespa64$igp, vespa64$igp[1])
  y <- append(vespa64$igp, vespa64$igp[[1]])
  expect_equal(x, y)
})

test_that("The function rnorm_qts() works", {
  withr::with_seed(1234, {
    expect_snapshot(rnorm_qts(1, vespa64$igp[[1]]))
  })
})

test_that("The function scale() works (center = TRUE, by_row = FALSE, keep_summary_stats = FALSE)", {
  qts_list <- scale(
    x = vespa64$igp,
    center = TRUE,
    scale = TRUE,
    by_row = FALSE,
    keep_summary_stats = FALSE
  )
  expect_snapshot(qts_list[[1]])
})

test_that("The function scale() works (center = FALSE, by_row = FALSE, keep_summary_stats = FALSE)", {
  qts_list <- scale(
    x = vespa64$igp,
    center = FALSE,
    scale = TRUE,
    by_row = FALSE,
    keep_summary_stats = FALSE
  )
  expect_equal(qts_list, vespa64$igp)
})

test_that("The function scale() works (center = FALSE, by_row = FALSE, keep_summary_stats = TRUE)", {
  qts_list <- scale(
    x = vespa64$igp,
    center = FALSE,
    scale = TRUE,
    by_row = FALSE,
    keep_summary_stats = TRUE
  )
  expect_equal(qts_list, list(
    rescaled_sample = vespa64$igp,
    mean_values = NA,
    sd_values = NA
  ))
})

test_that("The function scale() works (center = TRUE, by_row = TRUE, keep_summary_stats = FALSE)", {
  qts_list <- scale(
    x = vespa64$igp,
    center = TRUE,
    scale = TRUE,
    by_row = TRUE,
    keep_summary_stats = FALSE
  )
  expect_snapshot(qts_list[[1]])
})

test_that("The mean() method works for qts_sample objects", {
  expect_snapshot(mean(vespa64$igp))
})

test_that("The median() method works for qts_sample objects", {
  expect_snapshot(median(vespa64$igp))
})

test_that("Visualization code for QTS samples work", {
  p <- ggplot2::autoplot(vespa64$igp)
  expect_equal(dim(p$data), c(25856, 6))
  p <- ggplot2::autoplot(vespa64$igp, memberships = c(rep(1, 32), rep(2, 32)))
  expect_equal(dim(p$data), c(25856, 6))
  p <- ggplot2::autoplot(vespa64$igp, highlighted = c(TRUE, rep(FALSE, 63)))
  expect_equal(dim(p$data), c(404, 6))
})

test_that("Visualization functions for QTS work", {
  skip_if_not_installed("vdiffr")
  skip_on_covr()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "QTS sample plot",
    fig = plot(vespa64$igp)
  )
  vdiffr::expect_doppelganger(
    title = "QTS sample plot with memberships",
    fig = plot(vespa64$igp, memberships = c(rep(1, 32), rep(2, 32)))
  )
  vdiffr::expect_doppelganger(
    title = "QTS sample plot with highlighted observations",
    fig = plot(vespa64$igp, highlighted = c(TRUE, rep(FALSE, 63)))
  )
})
