test_that("test Format_BaHZING", {
  # Load data
  data("iHMP_Reduced")
  # Format microbiome data
  formatted_data <- Format_BaHZING(iHMP_Reduced)
  # Test format data
  testthat::expect_equal(object = length(formatted_data), expected = 6)
  testthat::expect_true(is.list(formatted_data))
  testthat::expect_true("Table" %in% names(formatted_data))
})



test_that("Error thrown when taxonomic levels are less than 2", {

  PS <- iHMP
  # Modify the taxonomic table to have only one level
  tax_table(PS) <- tax_table(tax_table(PS)[,1])

  # Expect that the function stops with an error message
  testthat::expect_error(Format_BaHZING(PS), "Need > 1 taxonomic level")
  })


test_that("If species level not present, create species column", {
  PS <- iHMP
  # phyloseq object without a 'Species' column
  tax_table(PS) <- tax_table(PS)[ , !colnames(tax_table(PS)) %in% "Species"]

  formatted_data <- Format_BaHZING(PS)

  # Extract the taxonomic table from the result
  taxa_table_result <- formatted_data$Table[[1]]

  # Check if 'Species' column is created
  testthat::expect_true(grepl("s__unclassified",
                              colnames(taxa_table_result)[ncol(taxa_table_result)]))
  testthat::expect_true(grepl("s__unclassified",
                              colnames(taxa_table_result)[ncol(taxa_table_result)-10]))
  testthat::expect_true(grepl("s__unclassified",
                              colnames(taxa_table_result)[ncol(taxa_table_result)-20]))
})

