# Define test fixture
test_fixture_4 <- list(
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
    test_file <- tempfile(pattern = "Trial_grp1_exp1_T0_", fileext = ".txt")
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
  Validity = c("invalid", rep("valid", 95)),
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

# Test case 1: Apply function to add metadata for horizontal axes and 'threshold' as validity method
test_that("tidy_plates_via_prompts() correctly adds metadata for horizontal axes using 'threshold' validity", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(rep(c("Group A",
                   "Experiment 1",
                   "threshold",
                   "1",
                   LETTERS[1:8],
                   paste(seq(from = 80, to = 10, length.out = 8))), times = 2),
                    collapse = "\n")
  write(input, f)
  
  # Apply setup action
  test_fixture_4$setup
  
  # Call function with list input
  tidy_data_horizontal_threshold <-  tidy_plates_via_prompts(
    test_list,
    direction = "horizontal",
    pattern = "Trial")
  
  # Call function with directory input
  tidy_file_horizontal_threshold <-  tidy_plates_via_prompts(
    test_directory,
    direction = "horizontal",
    pattern = "Trial")
  
  # Run test
  expect_identical(tidy_data_horizontal_threshold,
                   tidy_file_horizontal_threshold,
                   expected_result_horizontal)
  
  # Apply teardown action
  test_fixture_4$teardown
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})

 
# Test case 2: Apply function to add metadata for vertical axes and 'samples' as validity method
test_that("tidy_plates_via_prompts() correctly adds metadata for vertical axes using 'samples' validity", {

  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(rep(c("Group A",
                       "Experiment 1",
                       "samples",
                       "A-1",
                       LETTERS[1:12],
                       paste(seq(from = 120, to = 10, length.out = 12))), times = 2),
                 collapse = "\n")
  write(input, f)
  
  # Apply setup action
  test_fixture_4$setup

  # Call function with list input
  tidy_data_vertical_samples <-  tidy_plates_via_prompts(
    test_list,
    direction = "vertical",
    pattern = "Trial")

  # Call function with directory input
  tidy_file_vertical_samples <-  tidy_plates_via_prompts(
    test_directory,
    direction = "vertical",
    pattern = "Trial")

  # Run test
  expect_identical(tidy_data_vertical_samples,
                   tidy_file_vertical_samples,
                   expected_result_vertical_samples)

  # Apply teardown action
  test_fixture_4$teardown
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})
