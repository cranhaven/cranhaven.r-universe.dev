context("preprocess_org_names_urine")

test_that("It preprocesses organism names in urine samples correctly", {
  # Create a sample dataframe
  data <- data.frame(org_name = c("PRESUMPTIVELY Streptococcus",
                                  "MODERATE Escherichia coli",
                                  "S. AUREUS POSITIVE",
                                  "CANCELLED Influenza A"))

  # Call the function
  processed_data <- cleanse_urine_organism_names(data,
                                               column_name = "org_name",
                                               strings_to_remove = c("PRESUMPTIVELY",
                                                                     "MODERATE"),
                                               standard_mapping = c("S. AUREUS POSITIVE" = "STAPHYLOCOCCUS AUREUS"),
                                               filter_values = c("CANCELLED|INFLUENZA"))

    # Check if the preprocessing was done correctly

    # Check if strings_to_remove were removed
    testthat::expect_false(any(grepl("PRESUMPTIVELY|MODERATE", processed_data$org_name)))

    # Check if standard_mapping was applied correctly
    testthat::expect_false(any(grepl("S. AUREUS POSITIVE", processed_data$org_name)))

    # Check if filter_values were filtered out
    testthat::expect_false(any(grepl("CANCELLED|INFLUENZA", processed_data$org_name)))
})
