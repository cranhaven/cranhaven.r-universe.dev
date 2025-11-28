suppressMessages({

  # Skip all tests on CRAN
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
  }

  # Test 1A: Basic functionality works
  testthat::test_that("Basic functionality works", {

    unclean_names <- c("Pariis", "Marseill", "Lyone", "Toulous", "Niice")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )

    testthat::expect_equal(result, base_names)
  })

  # Test 1B: Basic functionality works
  testthat::test_that("Basic functionality works", {

    unclean_names <- c("Pariis", "Marseill", "Lyone", "Toulous", "Niice")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      user_base_only = T,
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )

    testthat::expect_equal(result, base_names)
  })

  # Test 2: Handles special characters and punctuation
  testthat::test_that("Handles special characters and punctuation", {
    unclean_names <- c("Par!is", "Marseill.", "Lyon@", "Tou'lous", "Nice#")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )
    testthat::expect_equal(result, base_names)
  })

  # Test 3: Report mode returns a data frame
  testthat::test_that("Report mode returns a data frame", {
    unclean_names <- c("Par!is", "Marseill.", "Lyon@", "Tou'lous", "Nice#")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names,
      report_mode = TRUE
    )
    testthat::expect_type(result, "list")
  })

  # Test 4: Handles missing values
  testthat::test_that("Handles missing values", {
    unclean_names <- c("Paris", "Marseille", "Lyon", NA, "Nice")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )
    testthat::expect_type(result, "character")
  })

  # Test 5: Is not case insensitive
  testthat::test_that("Case insensitive", {
    unclean_names <- c("paris", "MARSEILLE", "Lyon", "ToULouse", "nICE")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )
    testthat::expect_equal(result, base_names)
  })


  # Test 7: Test for Invalid Inputs
  testthat::test_that("clean_admin_names handles invalid country code", {
    testthat::expect_error(
      clean_admin_names(
        country_code = "INVALID",
        admin_names_to_clean = c("Nairobi")
      )
    )
  })


  # Test 6: Test for Invalid Inputs
  testthat::test_that("country_code and admin_names_to_clean are required", {
    # Test when both are NULL
    testthat::expect_error(
      clean_admin_names(
        country_code = NULL,
        admin_names_to_clean = NULL
      ),
      "Both 'country_code' and 'admin_names_to_clean' must be provided."
    )

    # Test when country_code is NULL
    testthat::expect_error(
      clean_admin_names(
        country_code = NULL,
        admin_names_to_clean = c("Paris", "Marseille")
      ),
      "'country_code' must be provided."
    )

    # Test when admin_names_to_clean is NULL
    testthat::expect_error(
      clean_admin_names(country_code = "FR", admin_names_to_clean = NULL),
      "'admin_names_to_clean' must be provided."
    )
  })

  # Test 8: Handles missing values
  testthat::test_that("Handles missing values", {
    unclean_names <- c("Paris", "Marseille", "Lyon", NA, "Nice")
    base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")

    result <- capture.output(
      clean_admin_names(
        country_code = "FR",
        report_mode = TRUE,
        user_base_admin_names = base_names,
        admin_names_to_clean = unclean_names
      ),
      type = "message"
    )

    expected <- paste(
      "There are 4 out of 4 (100%) admins",
      "that have been perfectly matched!"
    )

    testthat::expect_equal(writeLines(result), writeLines(expected))
  })

  # Test 10: Test that 'remove_words' works indirectly via 'clean_admin_names'
  testthat::test_that(
    "'remove_words' works indirectly through 'clean_admin_names'", {

      unclean_names <- c("Paris county", "Marseille city", "Lyon", NA, "Nice")
      base_names <- c("Paris", "Marseille", "Lyon", "Toulouse", "Nice")


    result <- clean_admin_names(
      country_code = "FR",
      user_base_admin_names = base_names,
      admin_names_to_clean = unclean_names
    )

    expected_result <- c("Paris", "Marseille", "Lyon", NA, "Nice")

    testthat::expect_equal(result, expected_result)
  })


})
