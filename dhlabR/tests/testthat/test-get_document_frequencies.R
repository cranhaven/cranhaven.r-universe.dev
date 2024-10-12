test_that("get_document_frequencies returns a valid dataframe", {
  # Use the `get_document_corpus` function to obtain a dataframe of document IDs
  document_ids_df <- get_document_corpus(doctype= "digibok")

  # Define input parameters for `get_document_frequencies`
  frequency_cutoff <- 10
  tokens <- c(".", ",", "hei")

  # Call the `get_document_frequencies` function with the obtained document IDs
  result <- get_document_frequencies(document_ids_df, frequency_cutoff, tokens)

  # Check that the result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check that the dataframe has the expected number of columns
  expect_equal(ncol(result), 4)

  # Check that the dataframe has the expected column names
  # expected_colnames <- c("Document_ID", "Token", "Token_frequency_in_the_document", "Total_tokens_in_the_document")
  # expect_equal(colnames(result), expected_colnames)

  # Check that the dataframe has at least one row (assuming there are matching documents)
  expect_gt(nrow(result), 0)
})
