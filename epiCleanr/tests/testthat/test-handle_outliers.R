
# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}


# get path
path <- system.file(
  "extdata",
  "fake_epi_df_togo.rds",
  package = "epiCleanr")

# get example data
fake_epi_df_togo <- import(path)

# drop na
fake_epi_df_togo2 <-  fake_epi_df_togo |> tidyr::drop_na()

# select cols
fake_epi_df_togo <- fake_epi_df_togo |>
  dplyr::select(tidyselect::matches("malaria|cholera"))

# Test 1: Check if outliers are identified correctly with Z-Score
testthat::test_that("Z-Score method identifies outliers correctly", {

  res <- handle_outliers(fake_epi_df_togo,
                                    vars = "malaria_cases",
                                    method = "zscore",
                                    report_mode = TRUE)

  testthat::expect_true(any(res$report$outliers == "5/900"))
})


# Test 2: Check if outliers are identified correctly with modified Z-Score
testthat::test_that("Modified Z-Score method identifies outliers correctly", {

  res <- handle_outliers(fake_epi_df_togo,
                                    vars = "malaria_cases",
                                    method = "mod_zscore",
                                    report_mode = TRUE)

  testthat::expect_true(any(res$report$outliers == "0/900"))
})



# Test 3: Check if outliers are identified correctly with IQR method
testthat::test_that("IQR method identifies outliers correctly", {

  res <- handle_outliers(fake_epi_df_togo,
                                    vars = "malaria_cases",
                                    method = "iqr_method",
                                    report_mode = TRUE)

  testthat::expect_true(any(res$report$outliers == "9/900"))
})



# Test 4: Check if outliers are removed correctly
testthat::test_that("Outliers are removed correctly", {

  exected_max <- fake_epi_df_togo |>
    tidyr::drop_na() |>
    summarise(max(malaria_cases)) |> dplyr::pull()

  res <- handle_outliers(fake_epi_df_togo,
                                    method = "zscore",
                                    treat_method = "remove")

  actual_max <-  res |>
    tidyr::drop_na() |>
    summarise(max(malaria_cases)) |> dplyr::pull()

  testthat::expect_true(all(exected_max > actual_max))
})


# Test 5: Check if report_mode returns a list with a dataframe and a ggplot
# object
testthat::test_that("Report mode returns correct types", {

  res <- handle_outliers(fake_epi_df_togo, report_mode = TRUE)

  testthat::expect_equal(typeof(res), as.character("list"))
  testthat::expect_equal(typeof(res$report), "list")
  testthat::expect_s3_class(res$plot, "ggplot")
})


# Test 5: Check if function correctly handles non-numeric columns
testthat::test_that("Function handles non-numeric columns", {

  res <- handle_outliers(fake_epi_df_togo2,
                                    vars = c("district", "malaria_tests"),
                                    report_mode = TRUE)

  testthat::expect_false("district" %in% colnames(res$report))
})


# Test 6: Check if outliers are replaced with NA when treat_method = "remove"
testthat::test_that(
  "Outliers are replaced with NA when treat_method is 'remove'", {


    res <- handle_outliers(fake_epi_df_togo2, method = "zscore",
                                      treat_method = "remove")

    testthat::expect_true(any(is.na(res$malaria_tests)))
    testthat::expect_true(any(is.na(res$malaria_cases)))
  })

# Test 7: Check if outliers are replaced with mean when treat_method = "mean"
testthat::test_that(
  "Outliers are replaced with mean when treat_method is 'mean'", {


    res <- handle_outliers(fake_epi_df_togo2,
                                      method = "zscore", treat_method = "mean")

    actual_mean_tests <- res |>
      dplyr::summarise(mean(malaria_tests)) |> dplyr::pull()

    expected_mean_tests <- fake_epi_df_togo2 |>
      dplyr::summarise(mean(malaria_tests)) |> dplyr::pull()

    testthat::expect_lt(as.numeric(actual_mean_tests),
                        as.numeric(expected_mean_tests))

  })


# Test 8: Check if outliers are replaced with median when treat_method ="median"
testthat::test_that(
  "Outliers are replaced with median when treat_method is 'mean'", {

    res <- handle_outliers(fake_epi_df_togo2,
                                      method = "zscore",
                           treat_method = "median")

    actual_mean_tests <- res |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()

    expected_mean_tests <- fake_epi_df_togo2 |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()

    testthat::expect_equal(as.numeric(actual_mean_tests),
                           as.numeric(expected_mean_tests))
  })


# Test 9: Check if outliers are replaced with quantile when
# treat_method="quantile"
testthat::test_that(
  "Outliers are replaced with median when treat_method is 'quantile'", {

    res <- handle_outliers(fake_epi_df_togo2,
                                      method = "zscore",
                                      treat_method = "quantile")

    actual_mean_tests <- res |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()

    expected_mean_tests <- fake_epi_df_togo2 |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()


    testthat::expect_equal(as.numeric(actual_mean_tests),
                           as.numeric(expected_mean_tests))
  })


# Test 10: Check if outliers are replaced with grouped_mean when
# treat_method="grouped_mean"
testthat::test_that(
  "Outliers are replaced with median when treat_method is 'grouped_mean'", {

    res <- handle_outliers(
      fake_epi_df_togo2,
      method = "zscore",
      grouping_vars = c("district", "year", "month"),
      treat_method = "grouped_mean")

    actual_mean_tests <- res |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()

    expected_mean_tests <- fake_epi_df_togo2 |>
      dplyr::summarise(median(malaria_tests)) |> dplyr::pull()


    testthat::expect_equal(as.numeric(actual_mean_tests),
                           as.numeric(expected_mean_tests))
  })


# Test 11: Check for error when grouped_mean is declared but no groups are given
testthat::test_that(
  "Grouped_mean is declared but no groups are given'", {

    testthat::expect_error(
      handle_outliers(fake_epi_df_togo,
                                 method = "zscore",
                                 treat_method = "grouped_mean")


    )

  })


# Test 12: Check for error when invalid treat method is given
testthat::test_that(
  "Check for error when invalid treat method is given", {

    testthat::expect_error(
      handle_outliers(fake_epi_df_togo,
                                 method = "zscore",
                                 treat_method = "multiple_imputation"),
      "Unknown treat_method: multiple_imputation"
    )
  })

# Test 13: Check 'grouped_mean' treatment in handle_outliers
testthat::test_that(
  "Check 'grouped_mean' treatment in handle_outliers function", {

    # Run the handle_outliers function with treat_method = "grouped_mean"
    result <- handle_outliers(
      fake_epi_df_togo2,
      method = "zscore",
      vars = "malaria_cases",
      grouping_vars = c("district", "year", "month"),
      treat_method = "grouped_mean")

    # Create what you expect grouped_mean_vals to look like
    expected_means <- fake_epi_df_togo2 %>%
      dplyr::group_by(district, year, month) %>%
      dplyr::reframe(
        dplyr::across(malaria_cases, \(x) mean(x, na.rm = TRUE))) |>
      dplyr::ungroup() |> dplyr::pull()


    exp <- as.numeric(mean(result$malaria_cases))
    act <- as.numeric(mean(expected_means))

    testthat::expect_true(exp < act)

  })

# Test 14: Check behavior when method is NULL or has length greater than 1
testthat::test_that(
  "Check behavior when method is NULL or has length greater than 1", {

    # Execute the function with method = NULL
    result1 <- handle_outliers(fake_epi_df_togo, method = NULL,
                                          report_mode = TRUE)

    title <-  paste0(
      "<span style='font-size:12pt; color:#21130D'>",
      "<b>Outlier Plot (Method:  Modified Z-Score )",
      "</b>: Outliers in <span style='color:#B44B1C'>orange</span>",
      ", Non-outliers in <span style='color:#1E81B0'>blue</span></span>"
    )

    # Validate that the chosen_df and title_suffix are as expected
    testthat::expect_equal(result1$plot$labels$title, title)

    testthat::expect_equal(result1$report$test[7], "Modified Z-Score")


    # Execute the function with method having a length greater than 1
    result2 <- handle_outliers(fake_epi_df_togo,
                                          method = c("zscore",
                                                     "modified_zscore"),
                                          report_mode = TRUE)

    title2 <-  paste0(
      "<span style='font-size:12pt; color:#21130D'>",
      "<b>Outlier Plot (Method:  Z-Score )",
      "</b>: Outliers in <span style='color:#B44B1C'>orange</span>",
      ", Non-outliers in <span style='color:#1E81B0'>blue</span></span>"
    )

    # Validate that the chosen_df and title_suffix are as expected
    testthat::expect_equal(result2$plot$labels$title, title2)
  })

