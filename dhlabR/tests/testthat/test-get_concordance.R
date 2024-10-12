test_that("get_concordance returns a valid dataframe", {
  # Use the `get_document_corpus` function to obtain a dataframe of document IDs
  document_ids_df <- get_document_corpus(doctype = "digibok")

  # Define input parameters for `get_concordance`
  #tokens <- c("word1", "word2", "word3")
  token = "Norge"
  window <- 20
  limit <- 1000

  # Call the `get_concordance` function with the obtained document IDs
  result <- get_concordance(document_ids_df, token, window, limit)

  # Check that the result is a dataframe
  expect_true(is.data.frame(result))

  # If the result is a dataframe, perform additional checks
  if (is.data.frame(result)) {
    # Check that the dataframe has the expected number of columns
   # expect_equal(ncol(result), length(tokens))

    # Check that the dataframe has at least one row (assuming there are matching concordance results)
    expect_gt(nrow(result), 0)
  }
})
