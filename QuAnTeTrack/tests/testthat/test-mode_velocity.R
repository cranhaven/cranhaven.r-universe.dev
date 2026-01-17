test_that("mode_velocity classifies MountTom dataset correctly", {
  # Hip heights for each track in the MountTom dataset
  H_mounttom <- c(
    1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
    1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784,
    1.676, 1.872, 1.648, 1.760, 1.612
  )

  # Simulate velocity data
  V_mounttom <- velocity_track(MountTom, H = H_mounttom)

  # Evaluate velocity trend
  result <- mode_velocity(V_mounttom)

  # Check that the result is a list with the same length as the input tracks
  expect_type(result, "list")
  expect_length(result, length(V_mounttom))

  # Ensure each element has the expected structure
  expect_true(all(sapply(result, function(x) is.list(x) || is.character(x))))

  # Verify that each track is classified into a valid trend category
  trends <- c("Acceleration", "Deceleration", "Steady", "Less than three steps")
  expect_true(all(sapply(result, function(x) if (is.list(x)) x$trend %in% trends else x %in% trends)))
})

test_that("mode_velocity classifies PaluxyRiver dataset correctly", {
  # Hip heights for each track in the PaluxyRiver dataset
  H_paluxyriver <- c(3.472, 2.200)

  # Calculation methods
  Method_paluxyriver <- c("A", "B")

  # Simulate velocity data
  V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

  # Evaluate velocity trend
  result <- mode_velocity(V_paluxyriver)

  # Check that the result is a list with the same length as the input tracks
  expect_type(result, "list")
  expect_length(result, length(V_paluxyriver))

  # Ensure each element has the expected structure
  expect_true(all(sapply(result, function(x) is.list(x) || is.character(x))))

  # Verify that each track is classified into a valid trend category
  trends <- c("Acceleration", "Deceleration", "Steady", "Less than three steps")
  expect_true(all(sapply(result, function(x) if (is.list(x)) x$trend %in% trends else x %in% trends)))
})

test_that("mode_velocity handles errors correctly", {
  # Input is not a list
  expect_error(mode_velocity("not a list"), "The 'trackvel' argument must be a list.")

  # List containing a track with fewer than 3 steps
  short_track <- list(c(0.1, 0.2)) # Only two values
  result <- mode_velocity(short_track)
  expect_equal(result[[1]], "Less than three steps")
})
