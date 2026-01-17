test_that("test_direction correctly performs ANOVA on MountTom dataset", {
  suppressWarnings(result <- test_direction(MountTom, analysis = "ANOVA"))
  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("ANOVA" %in% names(result))
})

test_that("test_direction correctly performs Kruskal-Wallis test on MountTom dataset", {
  result <- suppressWarnings(test_direction(MountTom, analysis = "Kruskal-Wallis"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("Kruskal_Wallis" %in% names(result))

  # Check that Kruskal-Wallis results contain required elements
  expect_true("Kruskal_Wallis" %in% names(result$Kruskal_Wallis))
  expect_true("Dunn" %in% names(result$Kruskal_Wallis))
})

test_that("test_direction correctly performs GLM on MountTom dataset", {
  suppressWarnings(result <- test_direction(MountTom, analysis = "GLM"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("GLM" %in% names(result))

  # Check that GLM results contain required elements
  expect_true("GLM" %in% names(result$GLM))
  expect_true("pairwise_results" %in% names(result$GLM))
})

test_that("test_direction correctly performs ANOVA on PaluxyRiver dataset", {
  result <- suppressWarnings(test_direction(PaluxyRiver, analysis = "ANOVA"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("ANOVA" %in% names(result))

  # Check that ANOVA results contain required elements
  expect_true("ANOVA" %in% names(result$ANOVA))
  expect_true("Tukey" %in% names(result$ANOVA))
})

test_that("test_direction correctly performs Kruskal-Wallis test on PaluxyRiver dataset", {
  result <- suppressWarnings(test_direction(PaluxyRiver, analysis = "Kruskal-Wallis"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("Kruskal_Wallis" %in% names(result))

  # Check that Kruskal-Wallis results contain required elements
  expect_true("Kruskal_Wallis" %in% names(result$Kruskal_Wallis))
  expect_true("Dunn" %in% names(result$Kruskal_Wallis))
})

test_that("test_direction correctly performs GLM on PaluxyRiver dataset", {
  result <- suppressWarnings(test_direction(PaluxyRiver, analysis = "GLM"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("GLM" %in% names(result))

  # Check that GLM results contain required elements
  expect_true("GLM" %in% names(result$GLM))
  expect_true("pairwise_results" %in% names(result$GLM))
})

test_that("test_direction gives an error for invalid analysis type", {
  expect_error(
    test_direction(MountTom, analysis = "InvalidMethod"),
    "Invalid 'analysis' argument. Choose from 'ANOVA', 'Kruskal-Wallis', or 'GLM'."
  )
})

test_that("test_direction gives an error when data is not a valid track object", {
  expect_error(
    test_direction(NULL, analysis = "ANOVA"),
    "The 'data' argument must be a 'track' R object, which is a list consisting of two elements: 'Trajectories' and 'Footprints'."
  )
  expect_error(
    test_direction(list(1, 2, 3), analysis = "ANOVA"),
    "Both elements of 'data' must be lists. Ensure that 'Trajectories' and 'Footprints' are provided."
  )
})

test_that("test_direction emits both expected warnings", {
  warnings <- capture_warnings(test_direction(MountTom, analysis = "ANOVA"))

  expect_true(any(grepl("The following tracks were removed from the analysis due to having 3 or fewer footprints: Track 05, Track 06, Track 10, Track 11, Track 12, Track 14, Track 17, Track 19, Track 20, Track 21, Track 22, Track 23.", warnings)))

  expect_true(any(grepl("One or more tracks do not follow a normal distribution \\(p-value <= 0.05\\). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.", warnings)))
})

test_that("test_direction gives an error when there are not enough valid tracks", {
  small_track_data <- subset_track(MountTom, tracks = c(1)) # Only one track
  expect_error(
    test_direction(small_track_data, analysis = "ANOVA"),
    "Not enough tracks with more than 3 footprints for meaningful analysis."
  )
})
