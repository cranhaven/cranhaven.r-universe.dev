library(testthat)


# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

suppressMessages(
  suppressWarnings(
    testthat::test_that("consistency_check function works correctly", {
      # Create a dummy dataset
      dummy_data <- data.frame(
        malaria_rdt_test = rnorm(1000, mean = 0, sd = 50),
        malaria_micro_test = rnorm(1000, mean = 0, sd = 10),
        malaria_rdt_cases = rnorm(1000, mean = 0, sd = 50),
        malaria_micro_cases = rnorm(1000, mean = 1, sd = 10)
      )

      # 1. Test when the length of tests and cases is not the same
      testthat::expect_error(
        consistency_check(dummy_data,
          tests = c("malaria_rdt_test"),
          cases = c(
            "malaria_rdt_cases",
            "malaria_micro_cases"
          )
        ),
        "The length of 'tests' and 'cases' must be the same."
      )

      # 2. Test when all the tests values are greater than the cases values
      tests_pass <- c("malaria_rdt_test", "malaria_micro_test")
      cases_pass <- c("malaria_rdt_cases", "malaria_micro_cases")

      dummy_data2 <- dummy_data |>
        mutate(
          malaria_rdt_test = abs(malaria_rdt_test) * 10000,
          malaria_micro_test = abs(malaria_micro_test) * 10000
        )

      actual <- as.character(
        testthat::capture_message(
          consistency_check(dummy_data2,
            tests = tests_pass,
            cases = cases_pass
          )
        )[1]
      )

      expected <- paste(
        "Consistency test passed for malaria_rdt_test",
        "vs malaria_rdt_cases: There are more tests than there are cases!"
      )

      testthat::expect_equal(writeLines(actual), writeLines(expected))


      # 3. Test when some tests values are less than the cases values
      dummy_data_with_inconsistency <- dummy_data
      dummy_data_with_inconsistency$malaria_micro_test[1] <- 1000

      expected <- consistency_check(dummy_data_with_inconsistency,
        tests = tests_pass, cases = cases_pass
      )

      expected <- paste(
        "Consistency test failed for malaria_micro_test",
        "vs malaria_micro_cases: There are 513 (51.3%) rows where tests",
        "are less than cases."
      )

      testthat::expect_equal(writeLines(actual), writeLines(expected))

      # 4. Test the return type of the function
      plot_result <- consistency_check(dummy_data,
        tests = tests_pass,
        cases = cases_pass
      )
      expect_equal(class(plot_result)[2], "ggplot")
    })
  )
)
