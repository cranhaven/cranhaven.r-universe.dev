# Define test fixture
test_fixture_1 <- list(
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
    test_file <- tempfile(pattern = "BMA_grp1_exp1_custom_T1_", fileext = ".txt")
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

test_that("read_plates() correctly reads input as expected", {
  
  # Apply setup action
  test_fixture_1$setup
  
  # Test case 1: Input from test directory with default pattern and skip_lines
  result_from_directory <- read_plates(test_directory)
  expect_true(is.list(result_from_directory))
  expect_true(all(sapply(result_from_directory, inherits, "data.frame")))

  # Test case 2: Input from test directory with custom pattern and default skip_lines
  custom_pattern <- "custom"
  custom_pattern_result <- read_plates(test_directory, pattern = custom_pattern)
  expect_true(is.list(custom_pattern_result))
  expect_true(all(sapply(custom_pattern_result, inherits, "data.frame")))

  # Test case 3: Input as list of data frames
  result_list <- read_plates(test_list)
  expect_identical(test_list, result_list)

  # # Test case 4: Input as invalid type
  expect_error(read_plates(123), "Input must be either a folder path to photometer files or a list of named data frames with 8 rows and 12 columns.")

  # Test case 5: Input as non-existent directory
  non_existent_folder <- "non-existent_folder"
  expect_true(length(read_plates(non_existent_folder)) == 0)

  # Test case 6: Input as folder path with no matching files
  expect_true(length(read_plates(test_directory, pattern="nonexistent_pattern")) == 0)
  
  # Apply teardown action
  test_fixture_1$teardown
})
