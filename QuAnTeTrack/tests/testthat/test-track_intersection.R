test_that("track_intersection correctly calculates intersection metrics without testing", {
  result <- track_intersection(PaluxyRiver, test = FALSE)

  # Check output structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), length(PaluxyRiver[[1]]))
  expect_equal(ncol(result), length(PaluxyRiver[[1]]))
})

test_that("track_intersection correctly calculates intersection metrics with testing (None)", {
  suppressWarnings(s1 <- simulate_track(PaluxyRiver, nsim = 10))
  result <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s1, origin.permutation = "None")

  # Check that result is a list with expected components
  expect_true(is.list(result))
  expect_true("Intersection_metric" %in% names(result))
  expect_true("Intersection_metric_p_values" %in% names(result))
  expect_true("Intersection_metric_p_values_combined" %in% names(result))
  expect_true("Intersection_metric_simulations" %in% names(result))
})

test_that("track_intersection correctly calculates intersection metrics with testing (Min.Box)", {
  suppressWarnings(s2 <- simulate_track(PaluxyRiver, nsim = 10))
  result <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s2, origin.permutation = "Min.Box")

  # Check structure
  expect_true(is.list(result))
  expect_true("Intersection_metric" %in% names(result))
  expect_true("Intersection_metric_p_values" %in% names(result))
})

test_that("track_intersection correctly calculates intersection metrics with testing (Conv.Hull)", {
  suppressWarnings(s3 <- simulate_track(PaluxyRiver, nsim = 10))
  result <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s3, origin.permutation = "Conv.Hull")

  # Check structure
  expect_true(is.list(result))
  expect_true("Intersection_metric" %in% names(result))
})

test_that("track_intersection correctly calculates intersection metrics with custom coordinate permutation", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  suppressWarnings(s5 <- simulate_track(sbMountTom, nsim = 10))
  area_origin <- matrix(
    c(
      50, 5,
      10, 5,
      10, 20,
      50, 20
    ),
    ncol = 2, byrow = TRUE
  )

  result <- track_intersection(sbMountTom,
    test = TRUE, H1 = "Higher", sim = s5, origin.permutation = "Custom",
    custom.coord = area_origin
  )

  expect_true(is.list(result))
  expect_true("Intersection_metric" %in% names(result))
})

test_that("track_intersection gives an error for invalid data input", {
  expect_error(track_intersection(NULL, test = FALSE), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
})

test_that("track_intersection gives an error for invalid origin permutation method", {
  expect_error(track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = suppressWarnings(simulate_track(PaluxyRiver, nsim = 10)), origin.permutation = "InvalidMethod"), "Invalid 'origin.permutation'. Valid options are: None, Min.Box, Conv.Hull, Custom")
})

test_that("track_intersection gives an error when test = TRUE but no sim data is provided", {
  expect_error(track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower"), "A 'sim' argument must be provided when 'test' is TRUE.")
})

test_that("track_intersection warns when custom.coord is missing for 'Custom' permutation", {
  expect_error(track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = suppressWarnings(simulate_track(PaluxyRiver, nsim = 10)), origin.permutation = "Custom"), "If 'origin.permutation' is set to 'Custom', the 'custom.coord' must be provided.")
})

test_that("track_intersection gives an error when custom.coord is not a matrix or dataframe", {
  expect_error(track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = suppressWarnings(simulate_track(PaluxyRiver, nsim = 10)), origin.permutation = "Custom", custom.coord = list(1, 2, 3)), "The 'custom.coord' must be a matrix or a data frame.")
})

test_that("track_intersection gives an error when custom.coord does not have two columns", {
  expect_error(track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = suppressWarnings(simulate_track(PaluxyRiver, nsim = 10)), origin.permutation = "Custom", custom.coord = matrix(1:9, ncol = 3)), "The 'custom.coord' must have exactly two columns.")
})
