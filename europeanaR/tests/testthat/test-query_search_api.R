test_that("Simple query is OK!", {
  skip_on_cran()
  resp <- query_search_api("arioch")
  expect_true(inherits(resp, "europeana_search_api"))
  expect_equal(resp$response$status_code, 200)
  expect_true(length(resp$content) > 1)
  expect_true(is.numeric(resp$content$itemsCount))
})

test_that("Simple query with refinment is OK!", {
  skip_on_cran()
  resp <- query_search_api("arioch", qf = "1712")
  expect_true(inherits(resp, "europeana_search_api"))
  expect_equal(resp$response$status_code, 200)
  expect_true(length(resp$content) > 1)
  expect_true(is.numeric(resp$content$itemsCount))
})

test_that("Simple query with refinment and media is OK!", {
  skip_on_cran()
  resp <- query_search_api("arioch", qf = "1712", media = TRUE)
  expect_true(inherits(resp, "europeana_search_api"))
  expect_equal(resp$response$status_code, 200)
  expect_true(length(resp$content) > 1)
  expect_true(is.numeric(resp$content$itemsCount))
})

