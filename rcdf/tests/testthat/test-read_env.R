test_that("read_env reads variables from .env file", {
  # Create a temporary .env file
  temp_env_file <- tempfile("test", fileext = ".env")
  writeLines(c("DB_HOST=localhost", "DB_USER=root", 'DB_PASS="secret"'), temp_env_file)

  # Read the .env file
  env_vars <- read_env(temp_env_file)

  # Check if the variables are read correctly
  expect_equal(env_vars$DB_HOST, "localhost")
  expect_equal(env_vars$DB_USER, "root")
  expect_equal(env_vars$DB_PASS, "secret")
  expect_equal(env_vars$DB_PORT, NULL)

  # Clean up temporary file
  unlink(temp_env_file)
})

test_that("read_env skips comments and empty lines", {
  # Create a temporary .env file with comments and empty lines
  temp_env_file <- tempfile("test", fileext = ".env")
  writeLines(c("# This is a comment", "", "DB_HOST=localhost", "# Another comment", "DB_USER=root"), temp_env_file)

  # Read the .env file
  env_vars <- read_env(temp_env_file)

  # Check if the comments and empty lines are ignored
  expect_equal(env_vars$DB_HOST, "localhost")
  expect_equal(env_vars$DB_USER, "root")

  # Clean up temporary file
  unlink(temp_env_file)
})

test_that("read_env returns NULL if no variables are found", {
  # Create a temporary empty .env file
  temp_env_file <- tempfile("test", fileext = ".env")
  writeLines(c("# This is a comment", ""), temp_env_file)

  # Read the .env file
  env_vars <- read_env(temp_env_file)

  # Check if it returns NULL
  expect_null(env_vars)

  # Clean up temporary file
  unlink(temp_env_file)
})

test_that("read_env defaults to .env in current directory if no path is provided", {
  # Create a temporary .env file in the current working directory
  dir_temp <- tempdir()
  temp_env_file <- file.path(dir_temp, '.env')
  writeLines(c("DB_HOST=localhost", "DB_USER=root"), temp_env_file)

  # Check that read_env can read the file without specifying path
  setwd(dir_temp)
  env_vars <- read_env()

  # Check if the variables are read correctly
  expect_equal(env_vars$DB_HOST, "localhost")
  expect_equal(env_vars$DB_USER, "root")

  # Clean up temporary file
  unlink(temp_env_file)
})

test_that("read_env throws an error when the file doesn't exist", {
  # A path that doesn't exist
  non_existent_file <- tempfile("nonexistent_file", fileext = ".env")

  # Expect error due to missing file
  expect_error(read_env(non_existent_file), "Path to environment file is required.")
})

