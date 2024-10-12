test_that("function returns a data frame", {
  result <- get_ngram_from_books()
  expect_s3_class(result, "data.frame")
})

test_that("function returns a data frame with the correct columns", {
  result <- get_ngram_from_books()
  expect_named(result, c("values", "ind"))
})
