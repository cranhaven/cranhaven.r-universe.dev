# Define test fixture
test_fixture_3 <- list(
  setup = {
    
    # Generate data for temporary file
    test_data <- "Line with additional information

   1  2  3  4  5  6  7  8  9 10 11 12
A  1  2  3  4  5  6  7  8  9 10 11 12
B 13 14 15 16 17 18 19 20 21 22 23 24
C 25 26 27 28 29 30 31 32 33 34 35 36
D 37 38 39 40 41 42 43 44 45 46 47 48
E 49 50 51 52 53 54 55 56 57 58 59 60
F 61 62 63 64 65 66 67 68 69 70 71 72
G 73 74 75 76 77 78 79 80 81 82 83 84
H 85 86 87 88 89 90 91 92 93 94 95 96"
    
    # Create a temporary directory to save temporary file
    test_file <- tempfile(pattern = "Assay_grp1_exp1_T0_", fileext = ".txt")
    test_file_name <- file_path_sans_ext(basename(test_file))
    test_directory <- tempdir()
    writeLines(test_data, test_file, sep = "\n")
  },
  teardown = {
    # Remove the temporary directory and its contents
    unlink("test_directory", recursive = TRUE)
  }
)

# Generate test data
m <- matrix(1:96, nrow = 8, byrow = TRUE)
row.names(m) <- LETTERS[1:8]
colnames(m) <- as.character(1:12)
test_df <- data.frame(m, check.names = F)
test_list <- list()
test_list[[test_file_name]] <- test_df
attr(test_list, "info") <- data.frame(File_name = test_file_name,
                                      Attribute = "Line with additional information")

expected_result_horizontal <- tibble(data.frame(
  Well_Position = paste0(rep(LETTERS[1:8],
                             each = length(1:12)), "-",
                         rep(1:12,
                             times = length(LETTERS[1:8]))),
  Value = 1:96,
  Validity = c("valid", rep("invalid", 95)),
  Treatment = rep(LETTERS[1:8], each = 12),
  Concentration = rep(seq(from = 80, to = 10,
                          length.out=8), each = 12),
  Timepoint = "T0",
  File = test_file_name,
  Group = "Group A",
  Experiment = "Experiment 1"
))
attr(expected_result_horizontal, "info") <- data.frame(File_name = test_file_name,
                                      Attribute = "Line with additional information")

expected_result_vertical <- tibble(data.frame(
  Well_Position = paste0(rep(LETTERS[1:8],
                             each = length(1:12)), "-",
                         rep(1:12,
                             times = length(LETTERS[1:8]))),
  Value = 1:96,
  Validity = c("valid", rep("invalid", 95)),
  Treatment = rep(LETTERS[1:12], times = 8),
  Concentration = rep(seq(from = 120, to = 10,
                          length.out=12), times = 8),
  Timepoint = "T0",
  File = test_file_name,
  Group = "Group A",
  Experiment = "Experiment 1"
))
attr(expected_result_vertical, "info") <- data.frame(File_name = test_file_name,
                                      Attribute = "Line with additional information")

# Test case 1: Apply function to list with data frames, metadata for horizontal axes,
# and 'threshold' as validity method
test_that("tidy_single_plate() correctly adds metadata for horizontal axes using 'threshold' validity", {
  
  # Apply setup action
  test_fixture_3$setup
  
  # Call function with list input
  tidy_data_horizontal_threshold <-  tidy_plates_via_params(
    test_list,
    direction = "horizontal",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "threshold",
    threshold = 1,
    treatment_labels = LETTERS[1:8],
    concentration_levels = seq(from = 80, to = 10, length.out = 8),
    pattern = "Assay")
  
  # Call function with directory input
  tidy_file_horizontal_threshold <-  tidy_plates_via_params(
    test_directory,
    direction = "horizontal",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "threshold",
    threshold = 1,
    treatment_labels = LETTERS[1:8],
    concentration_levels = seq(from = 80, to = 10, length.out = 8),
    pattern = "Assay")
  
  # Run test
  expect_identical(tidy_data_horizontal_threshold,
                   tidy_file_horizontal_threshold,
                   expected_result_horizontal)
  
  # Apply teardown action
  test_fixture_3$teardown
})

# Test case 2: Apply function to list with data frames, metadata for horizontal axes,
# and 'samples' as validity method
test_that("tidy_single_plate() correctly adds metadata for horizontal axes using 'samples' validity", {
  
  # Apply setup action
  test_fixture_3$setup
  
  # Call function with list input
  expected_result_horizontal_samples <- expected_result_horizontal %>% 
    mutate(Validity = c("invalid", rep("valid", 95)))
  tidy_data_horizontal_samples <-  tidy_plates_via_params(
    test_list,
    direction = "horizontal",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "samples",
    invalid_samples = "A-1",
    treatment_labels = LETTERS[1:8],
    concentration_levels = seq(from=80, to=10, length.out=8),
    pattern = "Assay")
  
  # Call function with directory input
  expected_file_horizontal_samples <-  tidy_plates_via_params(
    test_directory,
    direction = "horizontal",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "samples",
    invalid_samples = "A-1",
    treatment_labels = LETTERS[1:8],
    concentration_levels = seq(from=80, to=10, length.out=8),
    pattern = "Assay")
  
  # Run test
  expect_identical(tidy_data_horizontal_samples,
                   expected_file_horizontal_samples,
                   expected_result_horizontal_samples)
  
  # Apply teardown action
  test_fixture_3$teardown
})

# Test case 3: Apply function to list with data frames, metadata for vertical axes,
# and 'threshold' as validity method
test_that("tidy_single_plate() correctly adds metadata for vertical axes using 'threshold' validity.", {
  
  # Apply setup action
  test_fixture_3$setup
  
  # Call function with list input
  tidy_data_vertical_threshold <-  tidy_plates_via_params(
    test_list,
    direction = "vertical",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "threshold",
    threshold = 1,
    treatment_labels = LETTERS[1:12],
    concentration_levels = seq(from = 120, to = 10, length.out = 12),
    pattern = "Assay")
  
  # Call function with directory input
  tidy_file_vertical_threshold <-  tidy_plates_via_params(
    test_directory,
    direction = "vertical",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "threshold",
    threshold = 1,
    treatment_labels = LETTERS[1:12],
    concentration_levels = seq(from = 120, to = 10, length.out = 12),
    pattern = "Assay")
  
  # Run test
  expect_identical(tidy_data_vertical_threshold,
                   tidy_file_vertical_threshold,
                   expected_result_vertical)
  
  # Apply teardown action
  test_fixture_3$teardown
})

# Test case 4: Apply function to list with data frames, metadata for vertical axes,
# and 'samples' as validity method
test_that("tidy_single_plate() correctly adds metadata for vertical axes using 'samples' validity.", {
  
  # Apply setup action
  test_fixture_3$setup
  
  # Call function with list input
  expected_result_vertical_samples <- expected_result_vertical %>% 
    mutate(Validity = c("invalid", rep("valid", 95)))
  tidy_data_vertical_samples <-  tidy_plates_via_params(
    test_list,
    direction = "vertical",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "samples",
    invalid_samples = "A-1",
    treatment_labels = LETTERS[1:12],
    concentration_levels = seq(from = 120, to = 10, length.out = 12),
    pattern = "Assay")
  
  # Call function with directory input
  tidy_file_vertical_samples <-  tidy_plates_via_params(
    test_directory,
    direction = "vertical",
    group_ID = "Group A",
    experiment_name = "Experiment 1",
    validity_method = "samples",
    invalid_samples = "A-1",
    treatment_labels = LETTERS[1:12],
    concentration_levels = seq(from = 120, to = 10, length.out = 12),
    pattern = "Assay")
  
  # Run test
  expect_identical(tidy_data_vertical_samples,
                   tidy_file_vertical_samples,
                   expected_result_vertical_samples)
  
  # Apply teardown action
  test_fixture_3$teardown
})
