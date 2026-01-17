test_that("velocity_track correctly computes velocities and stride lengths for MountTom dataset", {
  # Define hip heights for MountTom dataset
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
    1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784,
    1.676, 1.872, 1.648, 1.760, 1.612
  )

  result <- velocity_track(MountTom, H = H_mounttom)

  # Check output structure
  expect_true(is.list(result))
  expect_equal(length(result), length(MountTom[[1]]))

  # Check that each track has expected parameter names
  expected_names <- c(
    "Step_velocities", "Mean_velocity", "Standard_deviation_velocity",
    "Maximum_velocity", "Minimum_velocity", "Step_relative_stride",
    "Mean_relative_stride", "Standard_deviation_relative_stride",
    "Maximum_relative_stride", "Minimum_relative_stride"
  )

  for (i in seq_along(result)) {
    expect_true(is.list(result[[i]]))
    expect_named(result[[i]], expected_names)
  }
})

test_that("velocity_track correctly computes velocities and stride lengths for PaluxyRiver dataset", {
  # Define hip heights for PaluxyRiver dataset
  H_paluxyriver <- c(3.472, 2.200)

  result <- velocity_track(PaluxyRiver, H = H_paluxyriver)

  # Check output structure
  expect_true(is.list(result))
  expect_equal(length(result), length(PaluxyRiver[[1]]))

  # Check expected parameter names
  expected_names <- c(
    "Step_velocities", "Mean_velocity", "Standard_deviation_velocity",
    "Maximum_velocity", "Minimum_velocity", "Step_relative_stride",
    "Mean_relative_stride", "Standard_deviation_relative_stride",
    "Maximum_relative_stride", "Minimum_relative_stride"
  )

  for (i in seq_along(result)) {
    expect_true(is.list(result[[i]]))
    expect_named(result[[i]], expected_names)
  }
})

test_that("velocity_track returns correct data types", {
  H_paluxyriver <- c(3.472, 2.200)
  result <- velocity_track(PaluxyRiver, H = H_paluxyriver)

  for (track in result) {
    expect_true(is.numeric(track$Step_velocities))
    expect_true(is.numeric(track$Mean_velocity))
    expect_true(is.numeric(track$Standard_deviation_velocity))
    expect_true(is.numeric(track$Maximum_velocity))
    expect_true(is.numeric(track$Minimum_velocity))
    expect_true(is.numeric(track$Step_relative_stride))
    expect_true(is.numeric(track$Mean_relative_stride))
    expect_true(is.numeric(track$Standard_deviation_relative_stride))
    expect_true(is.numeric(track$Maximum_relative_stride))
    expect_true(is.numeric(track$Minimum_relative_stride))
  }
})

test_that("velocity_track gives an error for non-list input", {
  expect_error(
    velocity_track(NULL, H = c(1.0)),
    "The 'data' argument must be a 'track' R object, which is a list consisting of two elements."
  )
  expect_error(
    velocity_track(42, H = c(1.0)),
    "The 'data' argument must be a 'track' R object, which is a list consisting of two elements."
  )
})

test_that("velocity_track gives an error for incorrectly structured data", {
  incorrect_data <- list(1:10, list())
  expect_error(
    velocity_track(incorrect_data, H = c(1.0)),
    "The two elements of 'data' must be lists."
  )
})

test_that("velocity_track handles incorrect hip height input", {
  expect_error(
    velocity_track(PaluxyRiver, H = NULL),
    "Error: 'H' must be a numeric vector with valid values."
  )
  expect_error(
    velocity_track(PaluxyRiver, H = c("wrong", "data")),
    "Error: 'H' must be a numeric vector with valid values."
  )
  expect_error(
    velocity_track(PaluxyRiver, H = c(1.0)),
    "Error: Length of 'H' must match the number of tracks in 'data'."
  )
})

test_that("velocity_track handles different velocity calculation methods", {
  H_paluxyriver <- c(3.472, 2.200)
  method_paluxyriver <- c("A", "B")
  result <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = method_paluxyriver)

  expect_true(is.list(result))
  expect_equal(length(result), length(PaluxyRiver[[1]]))

  expected_names <- c(
    "Step_velocities", "Mean_velocity", "Standard_deviation_velocity",
    "Maximum_velocity", "Minimum_velocity", "Step_relative_stride",
    "Mean_relative_stride", "Standard_deviation_relative_stride",
    "Maximum_relative_stride", "Minimum_relative_stride"
  )

  for (i in seq_along(result)) {
    expect_named(result[[i]], expected_names)
  }
})

test_that("velocity_track handles single-track data correctly", {
  single_track_data <- subset_track(PaluxyRiver, tracks = c(1))
  H_single <- c(3.472)
  result <- velocity_track(single_track_data, H = H_single)

  expect_true(is.list(result))
  expect_equal(length(result), 1)

  expected_names <- c(
    "Step_velocities", "Mean_velocity", "Standard_deviation_velocity",
    "Maximum_velocity", "Minimum_velocity", "Step_relative_stride",
    "Mean_relative_stride", "Standard_deviation_relative_stride",
    "Maximum_relative_stride", "Minimum_relative_stride"
  )

  expect_named(result[[1]], expected_names)
})

test_that("velocity_track handles multiple tracks correctly", {
  multi_track_data <- subset_track(PaluxyRiver, tracks = c(1, 2))
  H_multi <- c(3.472, 2.200)
  result <- velocity_track(multi_track_data, H = H_multi)

  expect_true(is.list(result))
  expect_equal(length(result), 2)

  expected_names <- c(
    "Step_velocities", "Mean_velocity", "Standard_deviation_velocity",
    "Maximum_velocity", "Minimum_velocity", "Step_relative_stride",
    "Mean_relative_stride", "Standard_deviation_relative_stride",
    "Maximum_relative_stride", "Minimum_relative_stride"
  )

  for (i in seq_along(result)) {
    expect_named(result[[i]], expected_names)
  }
})
