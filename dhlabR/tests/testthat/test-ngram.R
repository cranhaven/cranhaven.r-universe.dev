test_that("ngram function returns a data frame", {
  result <- ngram(word = "havet", corpus = "bok")
  expect_s3_class(result, "data.frame")
})
