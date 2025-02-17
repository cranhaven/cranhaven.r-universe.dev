test_that("The function differentiate() works", {
  expect_snapshot(differentiate(vespa64$igp[[1]]))
  expect_snapshot(differentiate(vespa64$igp))
})

test_that("The function straighten() works", {
  expect_snapshot(straighten(vespa64$igp[[1]]))
  expect_snapshot(straighten(vespa64$igp))
})

test_that("The function log() works", {
  expect_snapshot(log(vespa64$igp[[1]]))
  expect_snapshot(log(vespa64$igp))
})

test_that("The function exp() works", {
  x <- log(vespa64$igp[[1]])
  expect_equal(exp(x), vespa64$igp[[1]])
  x <- log(vespa64$igp)
  expect_equal(exp(x), vespa64$igp)
})

test_that("The function reorient() works", {
  expect_snapshot(reorient(vespa64$igp[[1]], disable_normalization = FALSE))
  expect_snapshot(reorient(vespa64$igp[[1]], disable_normalization = TRUE))
  expect_snapshot(reorient(vespa64$igp, disable_normalization = FALSE))
  expect_snapshot(reorient(vespa64$igp, disable_normalization = TRUE))
})

test_that("The function normalize() works", {
  expect_snapshot(normalize(vespa64$igp[[1]]))
  expect_snapshot(normalize(vespa64$igp))
})

test_that("The function resample() works", {
  expect_snapshot(resample(vespa64$igp[[1]]))
  expect_snapshot(resample(vespa64$igp))
})

test_that("The function smooth() works", {
  expect_snapshot(smooth(vespa64$igp[[1]]))
  expect_snapshot(smooth(vespa64$igp))
})

test_that("The function hemispherize() works", {
  expect_snapshot(hemispherize(vespa64$igp[[1]]))
  expect_snapshot(hemispherize(vespa64$igp))
})

test_that("The function moving_average() works", {
  expect_equal(moving_average(vespa64$igp[[1]]), vespa64$igp[[1]])
  expect_equal(moving_average(vespa64$igp), vespa64$igp)
})
