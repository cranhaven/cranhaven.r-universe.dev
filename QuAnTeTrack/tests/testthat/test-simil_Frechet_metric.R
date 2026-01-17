test_that("simil_Frechet_metric returns a valid similarity object for Directed model with PaluxyRiver dataset", {
  s1 <- simulate_track(PaluxyRiver, nsim = 10, model = "Directed")
  result <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")

  expect_type(result, "list")
  expect_true(all(c(
    "Frechet_distance_metric", "Frechet_distance_metric_p_values",
    "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations"
  ) %in% names(result)))
})

test_that("simil_Frechet_metric returns a valid similarity object for Constrained model with PaluxyRiver dataset", {
  s2 <- simulate_track(PaluxyRiver, nsim = 10, model = "Constrained")
  result <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s2, superposition = "None")

  expect_type(result, "list")
  expect_true(all(c(
    "Frechet_distance_metric", "Frechet_distance_metric_p_values",
    "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations"
  ) %in% names(result)))
})

test_that("simil_Frechet_metric returns a valid similarity object for Unconstrained model with PaluxyRiver dataset", {
  s3 <- simulate_track(PaluxyRiver, nsim = 10, model = "Unconstrained")
  result <- suppressWarnings(simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s3, superposition = "None"))

  expect_type(result, "list")
  expect_true(all(c(
    "Frechet_distance_metric", "Frechet_distance_metric_p_values",
    "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations"
  ) %in% names(result)))
})

test_that("simil_Frechet_metric returns a valid similarity object with Centroid superposition for MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  s4 <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))
  result <- suppressWarnings(simil_Frechet_metric(sbMountTom, test = TRUE, sim = s4, superposition = "Centroid"))

  expect_type(result, "list")
  expect_true(all(c(
    "Frechet_distance_metric", "Frechet_distance_metric_p_values",
    "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations"
  ) %in% names(result)))
})

test_that("simil_Frechet_metric returns a valid similarity object with Origin superposition for MountTom dataset", {
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  s5 <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))
  result <- suppressWarnings(simil_Frechet_metric(sbMountTom, test = TRUE, sim = s5, superposition = "Origin"))

  expect_type(result, "list")
  expect_true(all(c(
    "Frechet_distance_metric", "Frechet_distance_metric_p_values",
    "Frechet_metric_p_values_combined", "Frechet_distance_metric_simulations"
  ) %in% names(result)))
})

test_that("simil_Frechet_metric handles invalid inputs correctly", {
  expect_error(simil_Frechet_metric(NULL), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(simil_Frechet_metric(PaluxyRiver, test = "yes"), "'test' argument should be TRUE or FALSE.")
  expect_error(simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = NULL), "A 'sim' argument must be provided when 'test' is TRUE.")
  expect_error(simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = "list"), "The 'sim' argument must be a list.")
  expect_error(
    simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = suppressWarnings(simulate_track(PaluxyRiver, nsim = 10)), superposition = "Invalid"),
    "Invalid 'superposition' argument. One of 'None', 'Centroid', or 'Origin' must be chosen."
  )
})
