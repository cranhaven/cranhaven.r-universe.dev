test_that("query record API is OK!", {
  skip_on_cran()
  id <- "/2021618/resource_document_teylers_kunstverzamelingen_K_II_009"
  expect_error(resp <- query_record_api(id), NA)
  expect_true(inherits(resp, "europeana_record_api"))
  expect_equal(resp$response$status_code, 200)
  expect_true(length(resp$content) > 1)
  expect_true("object" %in% names(resp$content))
})
