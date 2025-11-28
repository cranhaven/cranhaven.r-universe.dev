test_that("known results", {
  expect_equal(
    ci2p(1.125, 0.634, 1.999, log_transform = TRUE),
    0.36790202
  )
  expect_equal(
    ci2p(1.257, 1.126, 1.403, log_transform = TRUE),
    0.0003118063,
    tolerance = 1e-7
  )
})

test_that("inverted lower and upper", {
  expect_warning(
    ci2p(1.125, 1.999, 0.634, log_transform = TRUE),
    "reversed"
  )
  expect_warning(
    ci2p(1.257, 1.403, 1.126, log_transform = TRUE),
    "reversed"
  )
  expect_equal(
    suppressWarnings(
      ci2p(1.125, 1.999, 0.634, log_transform = TRUE)
    ),
    0.36790202
  )
  expect_equal(
    suppressWarnings(
      ci2p(1.257, 1.403, 1.126, log_transform = TRUE)
    ),
    0.0003118063,
    tolerance = 1e-7
  )
})
