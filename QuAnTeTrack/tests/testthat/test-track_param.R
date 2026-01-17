test_that("track_param correctly computes parameters for PaluxyRiver dataset", {
  result <- track_param(PaluxyRiver)

  # Check output structure
  expect_true(is.list(result))
  expect_equal(length(result), length(PaluxyRiver[[1]]))

  # Check that each track has expected parameter names
  expected_names <- c(
    "Turning_angles", "Mean_turning_angle", "Standard_deviation_turning_angle",
    "Distance", "Length", "Step_lengths", "Mean_step_length",
    "Standard_deviation_step_length", "Sinuosity", "Straightness"
  )

  for (i in seq_along(result)) {
    expect_true(is.list(result[[i]]))
    expect_named(result[[i]], expected_names)
  }
})

test_that("track_param correctly computes parameters for MountTom dataset", {
  result <- track_param(MountTom)

  # Check output structure
  expect_true(is.list(result))
  expect_equal(length(result), length(MountTom[[1]]))

  # Check expected parameter names
  expected_names <- c(
    "Turning_angles", "Mean_turning_angle", "Standard_deviation_turning_angle",
    "Distance", "Length", "Step_lengths", "Mean_step_length",
    "Standard_deviation_step_length", "Sinuosity", "Straightness"
  )

  for (i in seq_along(result)) {
    expect_true(is.list(result[[i]]))
    expect_named(result[[i]], expected_names)
  }
})

test_that("track_param returns correct data types", {
  result <- track_param(PaluxyRiver)

  for (track in result) {
    expect_true(is.numeric(track$Turning_angles))
    expect_true(is.numeric(track$Mean_turning_angle))
    expect_true(is.numeric(track$Standard_deviation_turning_angle))
    expect_true(is.numeric(track$Distance))
    expect_true(is.numeric(track$Length))
    expect_true(is.numeric(track$Step_lengths))
    expect_true(is.numeric(track$Mean_step_length))
    expect_true(is.numeric(track$Standard_deviation_step_length))
    expect_true(is.numeric(track$Sinuosity))
    expect_true(is.numeric(track$Straightness))
  }
})

test_that("track_param handles empty data input", {
  empty_data <- list()
  expect_error(
    track_param(empty_data),
    "The 'data' argument must be a 'track' R object."
  )
})

test_that("track_param gives an error for non-list input", {
  expect_error(
    track_param(NULL),
    "The 'data' argument must be a 'track' R object."
  )
  expect_error(
    track_param(42),
    "The 'data' argument must be a 'track' R object."
  )
})

test_that("track_param gives an error for incorrectly structured data", {
  incorrect_data <- list(1:10, list())
  expect_error(
    track_param(incorrect_data),
    "Both elements of 'data' must be lists. Ensure that 'Trajectories' and 'Footprints' are provided."
  )
})

test_that("track_param handles single-track data correctly", {
  single_track_data <- subset_track(PaluxyRiver, tracks = c(1))
  result <- track_param(single_track_data)

  expect_true(is.list(result))
  expect_equal(length(result), 1)

  expected_names <- c(
    "Turning_angles", "Mean_turning_angle", "Standard_deviation_turning_angle",
    "Distance", "Length", "Step_lengths", "Mean_step_length",
    "Standard_deviation_step_length", "Sinuosity", "Straightness"
  )

  expect_named(result[[1]], expected_names)
})
