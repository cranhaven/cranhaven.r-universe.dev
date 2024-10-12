test_that("get_metadata returns a dataframe with expected columns", {
  # Replace these example dhlabid or urn values with valid ones for testing
  test_dhlabids <- c("100000000", "100000001")
  test_urns <- c("URN:NBN:no-nb_digibok_2011051604088")

  # Call the function with either test_dhlabids or test_urns
  metadata <- get_metadata(dhlabids = test_dhlabids)

  expected_columns <- c("dhlabid", "title", "authors", "urn", "oaiid", "sesamid", "isbn10", "city",
                        "timestamp", "year", "publisher", "langs", "subjects", "ddc", "genres", "literaryform",
                        "doctype", "ocr_creator", "ocr_timestamp")

  # Check if the returned dataframe has the expected columns
  expect_identical(colnames(metadata), expected_columns)
})
