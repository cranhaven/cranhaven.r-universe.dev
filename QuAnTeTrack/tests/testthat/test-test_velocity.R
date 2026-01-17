### **Prepare Data for Testing**
# Hip heights for each track in the MountTom dataset
H_mounttom <- c(
  1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600, 1.848,
  1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
  1.648, 1.760, 1.612
)

# Calculate velocities using the default Method "A"
V_mounttom <- velocity_track(MountTom, H = H_mounttom)

# Hip heights for each track in the PaluxyRiver dataset
H_paluxyriver <- c(3.472, 2.200)

# Specify different methods for different tracks in PaluxyRiver dataset
Method_paluxyriver <- c("A", "B")

# Calculate velocities using specified methods
V_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

### **Begin Tests**
test_that("test_velocity correctly performs ANOVA on MountTom dataset", {
  suppressWarnings(result <- test_velocity(MountTom, trackvel = V_mounttom, analysis = "ANOVA"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("ANOVA" %in% names(result))

  # Check that ANOVA results contain required elements
  expect_true("ANOVA" %in% names(result$ANOVA))
  expect_true("Tukey" %in% names(result$ANOVA))
})

test_that("test_velocity correctly performs Kruskal-Wallis test on MountTom dataset", {
  result <- suppressWarnings(test_velocity(MountTom, trackvel = V_mounttom, analysis = "Kruskal-Wallis"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("Kruskal_Wallis" %in% names(result))

  # Check that Kruskal-Wallis results contain required elements
  expect_true("Kruskal_Wallis" %in% names(result$Kruskal_Wallis))
  expect_true("Dunn" %in% names(result$Kruskal_Wallis))
})

test_that("test_velocity correctly performs GLM on MountTom dataset", {
  suppressWarnings(result <- test_velocity(MountTom, trackvel = V_mounttom, analysis = "GLM"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("GLM" %in% names(result))

  # Check that GLM results contain required elements
  expect_true("GLM" %in% names(result$GLM))
  expect_true("pairwise_results" %in% names(result$GLM))
})

test_that("test_velocity correctly performs ANOVA on PaluxyRiver dataset", {
  result <- suppressWarnings(test_velocity(PaluxyRiver, trackvel = V_paluxyriver, analysis = "ANOVA"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("ANOVA" %in% names(result))

  # Check that ANOVA results contain required elements
  expect_true("ANOVA" %in% names(result$ANOVA))
  expect_true("Tukey" %in% names(result$ANOVA))
})

test_that("test_velocity correctly performs Kruskal-Wallis test on PaluxyRiver dataset", {
  result <- suppressWarnings(test_velocity(PaluxyRiver, trackvel = V_paluxyriver, analysis = "Kruskal-Wallis"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("Kruskal_Wallis" %in% names(result))

  # Check that Kruskal-Wallis results contain required elements
  expect_true("Kruskal_Wallis" %in% names(result$Kruskal_Wallis))
  expect_true("Dunn" %in% names(result$Kruskal_Wallis))
})

test_that("test_velocity correctly performs GLM on PaluxyRiver dataset", {
  result <- suppressWarnings(test_velocity(PaluxyRiver, trackvel = V_paluxyriver, analysis = "GLM"))

  # Check for expected output structure
  expect_true("normality_results" %in% names(result))
  expect_true("homogeneity_test" %in% names(result))
  expect_true("GLM" %in% names(result))

  # Check that GLM results contain required elements
  expect_true("GLM" %in% names(result$GLM))
  expect_true("pairwise_results" %in% names(result$GLM))
})

test_that("test_velocity gives an error for invalid analysis type", {
  expect_error(
    test_velocity(MountTom, trackvel = V_mounttom, analysis = "InvalidMethod"),
    "Invalid 'analysis' type. Choose from 'ANOVA', 'Kruskal-Wallis', or 'GLM'."
  )
})

test_that("test_velocity gives an error when data is not a valid track object", {
  expect_error(
    test_velocity(NULL, trackvel = V_mounttom, analysis = "ANOVA"),
    "The 'data' argument must be a 'track' R object, which is a list consisting of two elements: 'Trajectories' and 'Footprints'."
  )
  expect_error(
    test_velocity(list(1, 2, 3), trackvel = V_mounttom, analysis = "ANOVA"),
    "The two elements of 'data' must be lists."
  )
})

### **Tests for Warnings**
test_that("test_velocity emits both expected warnings", {
  warnings <- capture_warnings(test_velocity(PaluxyRiver, trackvel = V_paluxyriver, analysis = "ANOVA"))

  expect_true(any(grepl("Homogeneity of variances assumption is violated \\(Levene's test p-value <= 0.05\\). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.", warnings)))

  expect_true(any(grepl("One or more tracks do not follow a normal distribution \\(p-value <= 0.05\\). Assumptions for ANOVA are not met. Consider using 'Kruskal-Wallis' or 'GLM'.", warnings)))
})

test_that("test_velocity gives an error when there are not enough valid tracks", {
  small_track_data <- subset_track(MountTom, tracks = c(1)) # Only one track
  expect_error(
    test_velocity(small_track_data, trackvel = V_mounttom, analysis = "ANOVA"),
    "Not enough tracks with more than 3 footprints for meaningful analysis."
  )
})

### **Extra Tests for Plotting**
test_that("test_velocity generates a boxplot with multiple tracks", {
  multi_track_data <- subset_track(MountTom, tracks = c(1, 2, 3, 4))
  multi_track_vel <- velocity_track(multi_track_data, H = rep(1.5, 4))

  expect_silent(test_velocity(multi_track_data, trackvel = multi_track_vel, plot = TRUE))
})
