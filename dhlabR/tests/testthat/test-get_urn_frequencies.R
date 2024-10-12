# Test 1: Test with a list of dhlabids
test_that("get_urn_frequencies works with a list of dhlabids", {
  # Create a list of dhlabids
  urn_list <- c(100243524, 100256010, 100597221)

  # Call the function with the list of dhlabids
  word_counts <- get_urn_frequencies(dhlabids = urn_list)

  # Check if the result is a data frame
  expect_s3_class(word_counts, "data.frame")

  # Check if the result has the correct column names
  expect_equal(colnames(word_counts), c("dhlabid", "tokens"))
})

# Test 2: Test with a data frame of URNs
test_that("get_urn_frequencies works with a data frame of URNs", {
  # Get a data frame of URNs using the 'get_document_corpus' function
  corpus <- get_document_corpus(doctype = "digibok")

  # Call the function with the data frame of URNs
  word_counts <- get_urn_frequencies(corpus)

  # Check if the result is a data frame
  expect_s3_class(word_counts, "data.frame")

  # Check if the result has the correct column names
  expect_equal(colnames(word_counts), c("dhlabid", "tokens"))
})


