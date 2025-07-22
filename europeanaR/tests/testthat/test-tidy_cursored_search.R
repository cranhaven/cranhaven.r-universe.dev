test_that("Cursored search is OK!", {
  skip_on_cran()
  max_items <- 20
  resp <- tidy_cursored_search(
    query = "animal",
    max_items = max_items,
    theme = "art",
    media = TRUE
  )
  expect_true(inherits(resp, "cursored_search"))
  expect_equal(resp$response$response$status_code, 200)
  expect_true(length(resp$response$content) > 1)
  expect_true(is.numeric(resp$response$content$itemsCount))
  expect_true(is.data.table(resp$data))
  expect_equal(nrow(resp$data), max_items)
  expect_true(ncol(resp$data) > 0)
  expect_true(all(c("id", "guid") %in% names(resp$data)))
})
