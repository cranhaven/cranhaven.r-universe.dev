# test_topic_modeling.R

library(testthat)

# Create a sample dataset for testing
sample_reviews <- data.frame(
  book_id = c("1", "2", "3"),
  reviewer_id = c("A", "B", "C"),
  review_content = c(
    "This book was great. I loved the characters and plot.",
    "Not a fan of this one. The story was too slow.",
    "An interesting read with complex themes."
  )
)

test_that("preprocess_reviews function works correctly", {
  result <- preprocess_reviews(sample_reviews, english_only = FALSE)

  expect_type(result, "list")
  expect_true("corpus" %in% names(result))
  expect_true("dtm" %in% names(result))
  expect_true("filtered_reviews" %in% names(result))

  expect_equal(nrow(result$filtered_reviews), 3)
})

test_that("fit_lda function works correctly", {
  preprocessed <- preprocess_reviews(sample_reviews, english_only = FALSE)
  lda_model <- fit_lda(preprocessed$dtm, k = 2)

  expect_s4_class(lda_model, "LDA")
  expect_true(inherits(lda_model, "TopicModel"))
  expect_equal(lda_model@k, 2)
})

test_that("top_terms function works correctly", {
  preprocessed <- preprocess_reviews(sample_reviews, english_only = FALSE)
  lda_model <- fit_lda(preprocessed$dtm, k = 2)

  expect_output(top_terms(lda_model, n = 5), "Topic 1:")
  expect_output(top_terms(lda_model, n = 5), "Topic 2:")
})

test_that("model_topics function works correctly", {
  result <- model_topics(sample_reviews, num_topics = 2, num_terms = 5, english_only = FALSE)

  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("filtered_reviews" %in% names(result))
  expect_s4_class(result$model, "LDA")
  expect_true(inherits(result$model, "TopicModel"))
  expect_equal(nrow(result$filtered_reviews), 3)
})
