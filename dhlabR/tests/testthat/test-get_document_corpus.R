test_that("get_document_corpus returns expected results", {
  # Test with default values
  result <- get_document_corpus()
  expect_s3_class(result, "data.frame")

  # Test with custom parameters
  result <- get_document_corpus(doctype = 'digibok', author = 'Henrik Ibsen', limit = 5)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= 5)

  # Test with an invalid doctype
  # Does not generate error
  # expect_error(get_document_corpus(doctype = "invalid_doctype"), "Error message from the API if any")
})
