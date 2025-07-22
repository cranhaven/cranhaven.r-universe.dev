test_that("Tidy response search response is OK!", {
  skip_on_cran()
  resp <- query_search_api("arioch")
  res <- tidy_search_items(resp = resp)
  expect_true(is.data.table(res))
  expect_true(nrow(res) > 0)
  expect_true(ncol(res) > 0)
  expect_true(all(c("id", "guid") %in% names(res)))
})
