test_that("Baseline", {

  set.seed(0)
  dm <- c(5,40,10,6)
  x <- array(rnorm(prod(dm))^2, dm)

  # trial-baseline
  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "percentage")
  exp <- aperm(
    apply(x, c(1,3,4), function(slice) {
      (slice / mean(slice[c(20:30)]) - 1) * 100
    }),
    c(2,1,3,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "sqrt_percentage")
  exp <- aperm(
    apply(x, c(1,3,4), function(slice) {
      slice <- sqrt(slice)
      (slice / mean(slice[c(20:30)]) - 1) * 100
    }),
    c(2,1,3,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "decibel")
  exp <- aperm(
    apply(x, c(1,3,4), function(slice) {
      slice <- 10 * log10(slice)
      slice - mean(slice[c(20:30)])
    }),
    c(2,1,3,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "zscore")
  exp <- aperm(
    apply(x, c(1,3,4), function(slice) {
      bl <- slice[c(20:30)]
      (slice - mean(bl)) / sd(bl)
    }),
    c(2,1,3,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "sqrt_zscore")
  exp <- aperm(
    apply(x, c(1,3,4), function(slice) {
      slice <- sqrt(slice)
      bl <- slice[c(20:30)]
      (slice - mean(bl)) / sd(bl)
    }),
    c(2,1,3,4)
  )
  expect_equal(act, exp)

  # trial-baseline, also with zero values

  x <- array(0, dm)
  exp <- x
  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "percentage")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "sqrt_percentage")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "decibel")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x + 1, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "zscore")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x + 1, along_dim = 2, unit_dims = c(1,3,4), baseline_indexpoints = c(20:30), method = "sqrt_zscore")
  expect_equal(act, exp)


  # global baseline
  set.seed(0)
  dm <- c(5,40,10,6)
  x <- array(rnorm(prod(dm))^2, dm)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "percentage")
  exp <- aperm(
    array(
      apply(x, c(1,4), function(slice) {
        (slice / mean(slice[c(20:30),]) - 1) * 100
      }),
      dm[c(2,3,1,4)]
    ),
    c(3,1,2,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "sqrt_percentage")
  exp <- aperm(
    array(
      apply(x, c(1,4), function(slice) {
        slice <- sqrt(slice)
        (slice / mean(slice[c(20:30),]) - 1) * 100
      }),
      dm[c(2,3,1,4)]
    ),
    c(3,1,2,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "decibel")
  exp <- aperm(
    array(
      apply(x, c(1,4), function(slice) {
        slice <- 10 * log10(slice)
        (slice - mean(slice[c(20:30),]))
      }),
      dm[c(2,3,1,4)]
    ),
    c(3,1,2,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "zscore")
  exp <- aperm(
    array(
      apply(x, c(1,4), function(slice) {
        bl <- slice[c(20:30),]
        (slice - mean(bl)) / sd(bl)
      }),
      dm[c(2,3,1,4)]
    ),
    c(3,1,2,4)
  )
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "sqrt_zscore")
  exp <- aperm(
    array(
      apply(x, c(1,4), function(slice) {
        slice <- sqrt(slice)
        bl <- slice[c(20:30),]
        (slice - mean(bl)) / sd(bl)
      }),
      dm[c(2,3,1,4)]
    ),
    c(3,1,2,4)
  )
  expect_equal(act, exp)

  # global-baseline, also with zero values

  x <- array(0, dm)
  exp <- x
  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "percentage")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "sqrt_percentage")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "decibel")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x + 1, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "zscore")
  expect_equal(act, exp)

  act <- ravetools::baseline_array(x = x + 1, along_dim = 2, unit_dims = c(1,4), baseline_indexpoints = c(20:30), method = "sqrt_zscore")
  expect_equal(act, exp)
})

