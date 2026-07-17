# Define test fixture
test_fixture_5 <- list(
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
    test_file <- tempfile(pattern = "Study_grp1_exp1_T0_", fileext = ".txt")
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
  Position = paste0(rep(LETTERS[1:8],
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
  Position = paste0(rep(LETTERS[1:8],
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

# Write tests using test_that blocks
test_that("tidy_plates() behaves as expected", {
  
  # Generate connection
  f <- file()
  options(microdiluteR.connection = f)
  input <- paste(c("Group A",
                   "Experiment 1",
                   "samples",
                   "A-1",
                   LETTERS[1:12],
                   paste(seq(from = 120, to = 10, length.out = 12))),
                 collapse = "\n")
  write(input, f)
  
  # Apply setup action
  test_fixture_5$setup
  
  # Test case 1: Test single plate with metadata in horizontal direction
  expected_result_single <- expected_result_horizontal
  attr(expected_result_single, "info") <- NULL
  tidy_data_single <- tidy_plates(test_list,
                                  how_many = "single",
                                  direction = "horizontal",
                                  group_ID = "Group A",
                                  experiment_name = "Experiment 1",
                                  validity_method = "threshold",
                                  threshold = 1,
                                  treatment_labels = LETTERS[1:8],
                                  concentration_levels = seq(from = 80, to = 10, length.out = 8),
                                  pattern = "Study")
  expect_identical(tidy_data_single, expected_result_single)
  
  # Test case 2: Test multiple plates without user prompts
  tidy_data_multiple_params <- tidy_plates(test_list,
                                           how_many = "multiple",
                                           direction = "vertical",
                                           group_ID = "Group A",
                                           experiment_name = "Experiment 1",
                                           validity_method = "samples",
                                           invalid_samples = "A-1",
                                           treatment_labels = LETTERS[1:12],
                                           concentration_levels = seq(from = 120, to = 10, length.out = 12),
                                           pattern = "Study")
  expect_equal(expected_result_vertical, tidy_data_multiple_params)
  
  # Test case 3: Test multiple plates tidy function with user prompts
  tidy_data_multiple_prompts <- tidy_plates(test_list,
                                           how_many = "multiple",
                                           user_prompt = T,
                                           direction = "vertical",
                                           pattern = "Study")
  expect_equal(expected_result_vertical, tidy_data_multiple_prompts)
  
  # Apply teardown action
  test_fixture_5$teardown
  
  # Reset connection
  options(microdiluteR.connection = stdin())
  # Close the file
  close(f)
})