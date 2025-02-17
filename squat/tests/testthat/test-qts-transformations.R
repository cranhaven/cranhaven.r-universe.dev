test_that("the function qts2dts() works", {
  expect_snapshot(qts2dts(vespa64$igp[[1]], vespa64$igp[[2]]))
})

test_that("the function qts2nts() works", {
  expect_snapshot(qts2nts(vespa64$igp[[1]], disable_normalization = FALSE))
  expect_snapshot(qts2nts(vespa64$igp[[1]], disable_normalization = TRUE))
})

test_that("the function qts2ats() works", {
  expect_snapshot(qts2ats(vespa64$igp[[1]], disable_normalization = FALSE))
  expect_snapshot(qts2ats(vespa64$igp[[1]], disable_normalization = TRUE))
})

test_that("the function qts2avts() works", {
  expect_snapshot(qts2avts(vespa64$igp[[1]], body_frame = FALSE))
  expect_snapshot(qts2avts(vespa64$igp[[1]], body_frame = TRUE))
})
