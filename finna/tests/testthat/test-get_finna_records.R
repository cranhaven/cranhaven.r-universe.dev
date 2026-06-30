test_that("get_finna_records function handles various cases correctly", {
  skip_on_cran()
  #skip_on_cran() # Skip on CRAN since it requires internet access

  # Test Case 1: Valid single ID with default options
  record_id <- "fikka.3405646"
  result <- get_finna_records(record_id)
  expect_true(nrow(result) == 1)
  expect_true(!is.null(result$Title))
  expect_true(!is.null(result$Author))

  # Test Case 2: Multiple IDs with default options
  record_ids <- c("fikka.3405646", "fikka.3405649")
  result <- get_finna_records(record_ids)
  expect_true(nrow(result) == 2)
  expect_true(!is.null(result$Title[1]))
  expect_true(!is.null(result$Title[2]))

  # Test Case 3: Non-existent ID should return an error
  invalid_id <- "fikka.invalid"
  expect_error(get_finna_records(invalid_id))

  # Test Case 4: Valid ID with specific fields
  result <- get_finna_records(record_id, field ="title")
  expect_true(nrow(result) == 1)
  expect_true("Title" %in% colnames(result))
  expect_true("Year" %in% colnames(result))
  expect_true("Author" %in% colnames(result)) # Author shouldn't be in the result

  # Test Case 5: Valid ID with pagination options
  record_ids <- "fikka.3405646"
  result <- get_finna_records(record_ids, limit = 2, page = 1)
  expect_true(nrow(result) == 1) # Only 2 records should be returned on page 1

  # Test Case 6: Valid ID with prettyPrint set to TRUE
  result <- get_finna_records(record_id, prettyPrint = TRUE)
  expect_true(nrow(result) == 1)
  expect_true(!is.null(result$Title))

  # Test Case 7: Invalid language option
  expect_error(get_finna_records(record_id, lng = "invalid_lang"))

  # Test Case 8: No IDs provided should return an error
  expect_error(get_finna_records(character(0)))

  # Test Case 9: Large number of IDs with pagination
  # record_ids <- paste0("fikka.", sample(1:10000, 100))
  # result <- get_finna_records(record_ids, limit = 10, page = 1)
  # expect_true(nrow(result) == 10)
})
