test_that("simulate_track returns a valid simulation object for default (Unconstrained) model with PaluxyRiver dataset", {
  suppressWarnings(result <- simulate_track(PaluxyRiver, nsim = 10))
  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Directed model with PaluxyRiver dataset", {
  result <- simulate_track(PaluxyRiver, nsim = 10, model = "Directed")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Constrained model with PaluxyRiver dataset", {
  result <- simulate_track(PaluxyRiver, nsim = 10, model = "Constrained")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Unconstrained model with PaluxyRiver dataset", {
  result <- simulate_track(PaluxyRiver, nsim = 10, model = "Unconstrained")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for default (Unconstrained) model with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  result <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Directed model with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  result <- simulate_track(sbMountTom, nsim = 10, model = "Directed")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Constrained model with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  result <- simulate_track(sbMountTom, nsim = 10, model = "Constrained")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track returns a valid simulation object for Unconstrained model with MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  result <- simulate_track(sbMountTom, nsim = 10, model = "Unconstrained")

  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
})

test_that("simulate_track warns when model is NULL and defaults to Unconstrained", {
  expect_warning(
    simulate_track(PaluxyRiver, nsim = 10, model = NULL),
    "`model` is NULL. Defaulting to 'Unconstrained'."
  )
})

test_that("simulate_track defaults to 1000 simulations when nsim is NULL", {
  suppressWarnings(result <- simulate_track(PaluxyRiver, nsim = NULL, model = "Unconstrained"))
  # Expect the number of simulated trajectory sets to be 1000
  expect_equal(length(result), 1000)
})

test_that("simulate_track handles invalid inputs correctly", {
  expect_error(simulate_track(NULL), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(simulate_track(PaluxyRiver, nsim = -5), "Error: `nsim` must be a positive integer.")
  expect_error(simulate_track(PaluxyRiver, nsim = 10, model = "InvalidModel"), "Error: Invalid `model` specified.")
})
