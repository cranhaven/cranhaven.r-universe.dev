fr1 <- as.data.table(fr1)
fr1 <- addNoiseAtEventTimes(fr1)

test_that("makeContWeights example produces expected output", {
  expect_length(fr1, 7)
  expect_equal(
    head(fr1$from),
    c(0.00, 0.41, 0.00, 0.65, 0.00, 0.37),
    ignore_attr = TRUE, tolerance = 1e-1
  )
  expect_equal(
    head(fr1$to),
    c(0.41, 0.47, 0.65, 3.33, 0.37, 1.44),
    ignore_attr = TRUE, tolerance = 1e-1
  )
})
