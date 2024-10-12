test_that("get_reference_words returns a valid list", {
  # Define input parameters for `get_reference_words`
  doctype <- "digibok"
  from_year <- 1900
  to_year <- 2000
  words <- c("Norge", "demokrati", "frihet")

  # Call the `get_reference_words` function with the specified parameters
  result <- get_reference_words(doctype, from_year, to_year, words)

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the list has the same length as the input words vector
  expect_equal(length(result), length(words))

  # Check that each element in the list is a named list with 'count' and 'relative_frequency' fields
  # for (word_result in result) {
  #   expect_true(is.list(word_result))
  #   expect_named(word_result, c("count", "relative_frequency"))
  # }
})
