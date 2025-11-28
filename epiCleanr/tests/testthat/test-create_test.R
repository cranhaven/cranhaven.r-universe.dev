
# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Create a fake data frame for testing purposes
fake_epi_data <- data.frame(
  month = rep(1:12, 1200),
  year = rep(2000:2011, each = 12 * 100),
  polio_tests = sample(0:122, 14400, replace = TRUE),
  stringsAsFactors = FALSE
)

suppressMessages({
  testthat::test_that("create_test handles dimensions correctly", {
    my_tests <- create_test(dimension_test = c(14400, 3))

    # Run the test function and capture the output
    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    # Expected message for passing test
    expt_pass <- paste(
      "Test passed! You have the correct",
      "number of dimensions!\n"
    )

    testthat::expect_equal(act, expt_pass)
  })


  testthat::test_that("create_test handles row duplicates correctly", {
    my_tests <- create_test(row_duplicates = TRUE)

    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    expt_pass <- paste(
      "Warning! Test failed. Duplicate rows",
      "found. See output$duplicate_rows.\n"
    )

    testthat::expect_equal(act, expt_pass)
  })


  testthat::test_that("create_test handles column duplicates correctly", {
    my_tests <- create_test(col_duplicates = TRUE)

    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    expt_pass <- paste("Test passed! No repeated columns found!\n")

    testthat::expect_equal(act, expt_pass)
  })



  testthat::test_that("create_test handles combinations correctly", {
    my_tests <- create_test(combinations_test = list(
      variables = c("month", "year"), expectation = 12 * 24
    ))

    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    expt_pass <- paste(
      "Warning! Test failed. Expected 288",
      "combinations but found 144 for month, year.\n"
    )

    testthat::expect_equal(act, expt_pass)
  })




  testthat::test_that("create_test handles min threshold correctly", {
    my_tests <- create_test(min_threshold_test = list(polio_tests = 0))

    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    expt_pass <- paste(
      "Test passed! Values in column polio_tests",
      "are above the threshold.\n"
    )

    testthat::expect_equal(act, expt_pass)
  })


  testthat::test_that("create_test handles max threshold correctly", {
    my_tests <- create_test(max_threshold_test = list(polio_tests = 122))

    result <- testthat::capture_message({
      my_tests(fake_epi_data)
    })[1]

    act <- spsUtil::remove_ANSI(result$message)

    expt_pass <- paste(
      "Test passed! Values in column polio_tests",
      "are below the threshold.\n"
    )

    testthat::expect_equal(act, expt_pass)
  })

  testthat::test_that("create_test uses big_mark correctly in error messages", {
    # Define a data frame that will fail the dimension test
    test_data <- data.frame(x = 1:10000, y = 1:10000)

    my_tests <- create_test(dimension_test = c(100001, 2))

    messages <- capture_messages(my_tests(test_data))[1]

    exp <- spsUtil::remove_ANSI(messages)

    actual <- paste(
      "Warning! Test failed. Expected 100,001 rows",
      "and 2 columns, but got 10,000 rows and 2 columns.\n"
    )

    testthat::expect_equal(actual, exp)
  })


  testthat::test_that(
    "test the output for for correct number of combinations", {
      # Test data with specific combinations
      test_data <- data.frame(
        column1 = c(1, 1, 2, 2),
        column2 = c("a", "b", "a", "b"),
        column3 = c(3, 3, 4, 4)
      )

      # Create the test function using the create_test function
      test_func <- create_test(
        combinations_test = list(
          variables = c("column1", "column2"),
          expectation = 4
        )
      )

      actual_error <- as.character(capture_message(test_func(test_data))[1])

      # Apply remove_ANSI to the captured message
      act <- spsUtil::remove_ANSI(actual_error)

      # Check if the test message is as expected
      expected <- paste(
        "Test passed! You have the correct number",
        "of combinations for column1, column2!\n"
      )

      testthat::expect_equal(act, expected)
    }
  )

  testthat::test_that(
    "create_test detects no repeated columns and returns success message",
    {
      test_data <- data.frame(x = 1:3, y = 4:6)

      my_tests <- create_test(col_duplicates = TRUE)

      messages <- capture_messages(my_tests(test_data))[1]

      # Apply remove_ANSI to the captured message
      act <- spsUtil::remove_ANSI(messages)

      expected <- "Test passed! No repeated columns found!\n"

      testthat::expect_equal(expected, act)
    }
  )

  testthat::test_that(
    "create_test throws an error when specified variables do not exist",
    {
      test_data <- data.frame(a = 1:3, b = 4:6)

      combinations_test <- list(variables = c("x", "y"), expectation = 6)

      my_tests <- create_test(combinations_test = combinations_test)

      actual_error <- as.character(capture_error(my_tests(test_data))[1])

      expected_error <-
        "Error! The following variables do not exist in the dataset: x, y"

      testthat::expect_equal(actual_error, expected_error)
    }
  )


  testthat::test_that(
    "create_test generates correct warning for failed combinations test",
    {
      # Create a data frame that will fail the combinations test
      test_data <- data.frame(month = 1:12, year = 2000:2003)

      combinations_test <- list(
        variables = c("month", "year"),
        expectation = 12 * 25
      )

      my_tests <- create_test(combinations_test = combinations_test)

      messages <- capture_messages(my_tests(test_data))[1]

      actual <- paste(
        "\033[31mWarning! Test failed. Expected 300 combinations",
        "but found 48 combinations for month, year.\033[39m\n"
      )

      testthat::expect_equal(writeLines(actual), writeLines(messages))
    }
  )

  testthat::test_that("create_test raises error if col does not exist in df", {
    # Create a data frame that lacks the specified column
    test_data <- data.frame(month = 1:12, year = 2000:2003)

    # Threshold test for a nonexistent column
    max_threshold_test <- list(nonexistent_column = 30)

    # Create a test function with the threshold_test
    my_tests <- create_test(max_threshold_test = max_threshold_test)

    actual_error <- as.character(capture_error(my_tests(test_data))[1])
    expected_error <-
      "Error! Column nonexistent_column does not exist in the dataset."
    testthat::expect_equal(actual_error, expected_error)
  })

  testthat::test_that(
    "test the portion of the code that checks for duplicate rows",
    {
      test_data <- data.frame(month = 1:12, year = 2000:2003)
      # Create the test function using the create_test function
      test_func <- create_test(row_duplicates = TRUE)

      actual_error <- as.character(capture_message(test_func(test_data))[1])
      message <- "Test passed! No duplicate rows found!"

      testthat::expect_equal(writeLines(actual_error), writeLines(message))
    }
  )

  testthat::test_that(
    "test the portion of the code that checks for duplicate cols",
    {
      test_data <- data.frame(month = 1:12, year = 2000:2003)
      # Create the test function using the create_test function
      test_func <- create_test(col_duplicates = TRUE)

      actual_error <- as.character(capture_message(test_func(test_data))[1])
      message <- "Test passed! No duplicate columns found!"

      testthat::expect_equal(writeLines(actual_error), writeLines(message))
    }
  )

  testthat::test_that(
    "test the output for for correct number of combinations",
    {
      # Test data with specific combinations
      test_data <- data.frame(
        column1 = c(1, 1, 2, 2),
        column2 = c("a", "b", "a", "b"),
        column3 = c(3, 3, 4, 4)
      )

      # Create the test function using the create_test function
      test_func <- create_test(
        combinations_test = list(
          variables = c("column1", "column2"),
          expectation = 4
        )
      )

      actual_error <- as.character(capture_message(test_func(test_data))[1])

      # Check if the test message is as expected
      expected <- paste(
        "Test passed! You have the correct number",
        "of combinations for column1, column2!"
      )

      testthat::expect_equal(writeLines(actual_error), writeLines(expected))
    }
  )

  testthat::test_that(
    "create_test warns about values failing the threshold",
    {
      # Test data with specific values
      test_data <- data.frame(column1 = c(5, 10, 15, 20))

      # Define the column and threshold
      max_threshold_test <- list(column1 = 18)

      # Create the test function using the create_test function
      my_tests <- create_test(max_threshold_test = max_threshold_test)

      # Capture the message and convert it to a character
      actual_error <- capture_message(my_tests(test_data)[1])

      act <- gsub("\\n", "", as.character(actual_error$message))
      act <- trimws(act) # Remove leading and trailing whitespace


      # Check if the warning message is as expected
      expected_warning <- paste(
        "Warning! Test failed. Values in column",
        "column1 are above the threshold. See output$max_thresh_column1."
      )

      expt_pass <- trimws(act)

      # Expectation for the warning message
      testthat::expect_equal(act, expt_pass)

    }
  )


  testthat::test_that(
    "create_test sets percentage_passed to NULL when total_tests is 0",
    {
      my_tests <- create_test()
      test_data <- data.frame(column1 = c(5, 10, 15, 20))
      actual_error <- as.character(capture_error(my_tests(test_data))[1])
      expected <- "No tests have been carried out!"

      testthat::expect_equal(actual_error, expected)
    }
  )

  testthat::test_that(
    "create_test warns about repeated columns",
    {
      # Create a test data frame with duplicate columns
      test_data <- data.frame(a = 1:3, b = 4:6, b = 4:6)

      # Create the test function
      my_tests <- create_test(col_duplicates = TRUE)

      # Capture the warning message
      actual <- as.character(capture_message(my_tests(test_data))[1])

      # Expected warning message
      expected <- paste(
        "Warning! Test failed. Repeated columns found.",
        "See output$duplicate_columns."
      )

      # Check if the actual warning matches the expected warning
      testthat::expect_equal(writeLines(actual), writeLines(expected))
    }
  )

  testthat::test_that(
    "create_test warns about values exceeding the threshold",
    {
      test_data <- data.frame(a = c(1, 5, 3), b = c(4, 6, 2))

      # Define the threshold test for the "a" column, with a max threshold of 4
      max_threshold_test <- list(a = 4)

      # Create the test function
      my_tests <- create_test(max_threshold_test = max_threshold_test)

      # Capture the warning message
      actual <- as.character(capture_message(my_tests(test_data))[1])

      # Expected warning message
      expected <- paste(
        "Warning! Test failed. Values in column a are",
        "above the threshold. See output$max_thresh_a."
      )

      # Check if the actual warning matches the expected warning
      testthat::expect_equal(writeLines(actual), writeLines(expected))
    }
  )



  if (Sys.info()['sysname'] != 'Linux' || grepl("UTF-8", Sys.getenv("LANG"))) {

    testthat::test_that(
      "create_test returns praise emoji when all tests pass", {
      # Test data and parameters
      test_data <- data.frame(a = 1:3, b = 4:6)
      dimension_test <- c(3, 2)

      # Create the test function
      my_tests <- create_test(dimension_test = dimension_test)

      # Capture the messages
      messages <- as.character(capture_messages(my_tests(test_data)[2]))

      # Extract the final message
      final_message <- messages[length(messages)]

      # Expected emojis
      expected_emojis <- c(
        "\U0001f600", # smile
        "\U0001f973", # party face
        "\U0001f638", # cat grin
        "\U0001f308", # rainbow
        "\U0001f947", # gold medal
        "\U0001f389", # party popper
        "\U0001f38a" # confetti ball
      )

      # Check if the final message contains any of the expected emojis
      testthat::expect_true(any(sapply(expected_emojis, grepl, final_message)))
    })

  }
})
