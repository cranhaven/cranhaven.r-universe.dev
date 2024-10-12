# Create a test for the get_collocations function
test_that("get_collocations returns a data frame", {
  corpus <- get_document_corpus(doctype="digibok")
  # pids <- c("pid1", "pid2", "pid3")
  word <- "."

  # You may need to replace the pids and word with valid values to ensure the test works as expected
  result <- get_collocations(corpus, word)

  expect_s3_class(result, "data.frame")
})
