test_that("combined_prob() correctly handles invalid inputs", {
  # Incorrect test data
  expect_error(combined_prob(NULL, metrics = list()), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(combined_prob(list(), metrics = list()), "The 'data' argument must be a 'track' R object, which is a list consisting of two elements.")
  expect_error(combined_prob(PaluxyRiver, metrics = NULL), "'metrics' must be provided and should be a list of track similarity or intersection metrics.")

  # Metrics with different numbers of simulations
  s_fake <- simulate_track(PaluxyRiver, nsim = 10, model = "Directed")
  DTW_fake <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s_fake, superposition = "None")
  Frechet_fake <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s_fake, superposition = "None")

  # Create a metric with a different number of simulations
  s_fake2 <- simulate_track(PaluxyRiver, nsim = 9, model = "Directed")
  int_fake <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s_fake2, origin.permutation = "None")

  expect_error(combined_prob(PaluxyRiver, metrics = list(DTW_fake, Frechet_fake, int_fake)), "All elements in 'metrics' must have the same number of simulations.")
})

test_that("combined_prob() correctly computes combined probabilities", {
  # Example 1: Simulating with the "Directed" model
  s1 <- simulate_track(PaluxyRiver, nsim = 10, model = "Directed")
  DTW1 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
  Frechet1 <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s1, superposition = "None")
  int1 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s1, origin.permutation = "None")
  result1 <- combined_prob(PaluxyRiver, metrics = list(DTW1, Frechet1, int1))

  expect_true(is.list(result1))
  expect_named(result1, c(
    "P_values (DTW_distance_metric, Frechet_distance_metric, Intersection_metric)",
    "P_values_combined (DTW_distance_metric, Frechet_distance_metric, Intersection_metric)"
  ))
  expect_true(all(result1[[1]] >= 0 & result1[[1]] <= 1, na.rm = TRUE))
  expect_true(result1[[2]] >= 0 & result1[[2]] <= 1)
  names(result1)
  # Example 2: Simulating with the "Constrained" model
  s2 <- simulate_track(PaluxyRiver, nsim = 10, model = "Constrained")
  DTW2 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s2, superposition = "None")
  Frechet2 <- simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s2, superposition = "None")
  int2 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s2, origin.permutation = "Min.Box")
  result2 <- combined_prob(PaluxyRiver, metrics = list(DTW2, Frechet2, int2))

  expect_true(is.list(result2))
  expect_true(all(result2[[1]] >= 0 & result2[[1]] <= 1, na.rm = TRUE))
  expect_true(result2[[2]] >= 0 & result2[[2]] <= 1)

  # Example 3: Simulating with the "Unconstrained" model
  s3 <- simulate_track(PaluxyRiver, nsim = 10, model = "Unconstrained")
  DTW3 <- simil_DTW_metric(PaluxyRiver, test = TRUE, sim = s3, superposition = "None")
  Frechet3 <- suppressWarnings(simil_Frechet_metric(PaluxyRiver, test = TRUE, sim = s3, superposition = "None"))
  int3 <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", sim = s3, origin.permutation = "Conv.Hull")
  result3 <- combined_prob(PaluxyRiver, metrics = list(DTW3, Frechet3, int3))

  expect_true(is.list(result3))
  expect_true(all(result3[[1]] >= 0 & result3[[1]] <= 1, na.rm = TRUE))
  expect_true(result3[[2]] >= 0 & result3[[2]] <= 1)

  # Example 4: Simulating tracks in MountTom using the "Centroid" superposition method
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  s4 <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))
  DTW4 <- simil_DTW_metric(sbMountTom, test = TRUE, sim = s4, superposition = "Centroid")
  Frechet4 <- suppressWarnings(simil_Frechet_metric(sbMountTom, test = TRUE, sim = s4, superposition = "Centroid"))
  int4 <- track_intersection(sbMountTom, test = TRUE, H1 = "Higher", sim = s4, origin.permutation = "Min.Box")
  result4 <- combined_prob(sbMountTom, metrics = list(DTW4, Frechet4, int4))

  expect_true(is.list(result4))
  expect_true(all(result4[[1]] >= 0 & result4[[1]] <= 1, na.rm = TRUE))
  expect_true(result4[[2]] >= 0 & result4[[2]] <= 1)

  # Example 5: Simulating tracks in MountTom using the "Origin" superposition method
  sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
  s5 <- suppressWarnings(simulate_track(sbMountTom, nsim = 10))
  DTW5 <- simil_DTW_metric(sbMountTom, test = TRUE, sim = s5, superposition = "Origin")
  Frechet5 <- suppressWarnings(simil_Frechet_metric(sbMountTom, test = TRUE, sim = s5, superposition = "Origin"))
  area_origin <- matrix(c(50, 5, 10, 5, 10, 20, 50, 20), ncol = 2, byrow = TRUE)
  int5 <- track_intersection(sbMountTom,
    test = TRUE, H1 = "Higher", sim = s5,
    origin.permutation = "Custom", custom.coord = area_origin
  )
  result5 <- combined_prob(sbMountTom, metrics = list(DTW5, Frechet5, int5))

  expect_true(is.list(result5))
  expect_true(all(result5[[1]] >= 0 & result5[[1]] <= 1, na.rm = TRUE))
  expect_true(result5[[2]] >= 0 & result5[[2]] <= 1)
})
